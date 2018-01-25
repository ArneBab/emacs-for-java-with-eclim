#!/usr/bin/env python
"""A hacked up version of the multiple-Python checkers script from EmacsWiki.

Original work taken from http://www.emacswiki.org/emacs/PythonMode, author
unknown.

Further extended by Jason Kirtland <jek@discorporate.us> under the Creative
Commons Share Alike 1.0 license:
http://creativecommons.org/licenses/sa/1.0/

Later improvements by Marc Sherry <msherry@gmail.com>
"""

from __future__ import absolute_import, division, print_function

from argparse import ArgumentParser, ArgumentTypeError
from functools import partial
import os
import re
from subprocess import call, Popen, PIPE
import sys

# TODO: Ignore the type of ConfigParser until
# https://github.com/python/mypy/issues/1107 is fixed
try:
    from configparser import ConfigParser  # type: ignore
except ImportError:
    from ConfigParser import SafeConfigParser as ConfigParser  # type: ignore

try:
    # pylint: disable=unused-import, ungrouped-imports
    from argparse import Namespace
    from typing import (
        Any, Dict, List, IO, Optional, Set, Tuple)
except ImportError:
    pass

CONFIG_FILE_NAME = '.pycheckers'

# Checkers to run by default, when no --checkers options are supplied.
default_checkers = 'pylint,mypy2,mypy3'


class FatalException(Exception):
    def __init__(self, msg, filename):
        self.msg = msg
        self.filename = filename
        super(FatalException, self).__init__()

    def __str__(self):
        return 'ERROR :pycheckers:{msg} at {filename} line 1.'.format(
            msg=self.msg, filename=self.filename)


def is_true(v):
    # type: (str) -> bool
    return v.lower() in {'yes', 'true', 't', 'y', 'on', '1'}


def is_false(v):
    # type: (str) -> bool
    return v.lower() in {'no', 'false', 'f', 'n', 'off', '0'}


def str2bool(v):
    # type: (str) -> bool
    if is_true(v):
        return True
    elif is_false(v):
        return False
    raise ArgumentTypeError('Boolean value expected.')


def croak(msgs, filename):
    # type: (Tuple[str], str) -> None
    for m in msgs:
        print('ERROR :pycheckers:{} at {} line 1.'.format(m.strip(), filename), file=sys.stderr)
    sys.exit(1)


class LintRunner(object):
    """Base class provides common functionality to run python code checkers."""

    out_fmt = ("%(level)s %(error_type)s%(error_number)s:"
               "%(description)s at %(filename)s line %(line_number)s.")
    out_fmt_w_col = (
        "%(level)s %(error_type)s%(error_number)s:"
        "%(description)s at %(filename)s line %(line_number)s,"
        "%(column_number)s.")

    output_template = dict.fromkeys(
        ('level', 'error_type', 'error_number', 'description',
         'filename', 'line_number', 'column_number'), '')

    output_matcher = re.compile(r'')

    command = ''

    def __init__(self, ignore_codes, enable_codes, options):
        # type: (Tuple[str], Tuple[str], Namespace) -> None
        self.ignore_codes = set(ignore_codes)
        self.enable_codes = set(enable_codes)
        self.options = options

    @property
    def name(self):
        # type: () -> str
        """The linter's name, which is usually the same as the command.

        They may be different if there are multiple versions run with
        flags -- e.g. the MyPy2Runner's name may be 'mypy2', even though
        the command is just 'mypy'.
        """
        return self.command

    def get_run_flags(self, _filename):
        # type: (str) -> Tuple[str, ...]
        return ()

    def get_env_vars(self):
        # type: () -> Dict[str, str]
        return {}

    def fixup_data(self, _line, data):
        # type: (str, Dict[str, str]) -> Dict[str, str]
        return data

    def process_output(self, line):
        # type: (str) -> Optional[Dict[str, str]]
        m = self.output_matcher.match(line)
        if m:
            return m.groupdict()
        return None

    def process_returncode(self, _returncode):
        # type: (int) -> bool
        """Return True if the checker's returncode indicates successful check, False otherwise"""
        return True

    def _process_streams(self, *streams):
        # type: (*List[str]) -> Tuple[int, List[str]]
        """This runs over both stdout and stderr, counting errors/warnings."""
        if not streams:
            raise ValueError('No streams passed to _process_streams')
        errors_or_warnings = 0
        out_lines = []
        for stream in streams:
            for line in stream:
                match = self.process_output(line)
                if match:
                    tokens = dict(self.output_template)
                    # Return None from fixup_data to ignore this error
                    fixed_up = self.fixup_data(line, match)
                    if fixed_up:
                        # Prepend the command name to the description (if
                        # present) so we know which checker threw which error
                        if 'description' in fixed_up:
                            fixed_up['description'] = '%s: %s' % (
                                self.name, fixed_up['description'])
                        tokens.update(fixed_up)
                        template = (
                            self.out_fmt_w_col if fixed_up.get('column_number')
                            else self.out_fmt)
                        out_lines.append(template % tokens)
                        errors_or_warnings += 1
        return errors_or_warnings, out_lines

    def _executable_exists(self):
        # type: () -> bool
        # https://stackoverflow.com/a/6569511/52550
        args = ['/usr/bin/env', 'which', self.command]
        try:
            process = Popen(args, stdout=PIPE, stderr=PIPE)
        except Exception as e:                   # pylint: disable=broad-except
            print(e)
            return False
        exec_path, _err = process.communicate()

        args = ['[', '-x', exec_path.strip(), ']']
        retcode = call(args)
        return retcode == 0

    def run(self, filename):
        # type: (str) -> Tuple[int, List[str]]
        if not self._executable_exists():
            # Return a parseable error message so the normal parsing mechanism
            # can display it
            return 1, [
                ('ERROR : {}:Checker not found on PATH, '
                 'unable to check at {} line 1.'.format(
                     self.command, filename))]

        # `env` to use a virtualenv, if found
        args = ['/usr/bin/env', self.command]
        try:
            args.extend(self.get_run_flags(filename))
            args.append(filename)
            process = Popen(
                args, stdout=PIPE, stderr=PIPE, universal_newlines=True,
                env=dict(os.environ, **self.get_env_vars()))
        except Exception as e:                   # pylint: disable=broad-except
            print(e, args)
            return 1, [str(e)]

        out, err = process.communicate()
        process.wait()
        errors_or_warnings, out_lines = self._process_streams(
            out.splitlines(), err.splitlines())

        if not self.process_returncode(process.returncode):
            errors_or_warnings += 1
            out_lines += [
                ('WARNING : {}:Checker indicated failure of some kind at {} line 1.'.format(
                    self.command, filename))]
            if self.options.report_checker_errors_inline:
                for line in err.splitlines():
                    out_lines += ['WARNING : {}:{} at {} line 1.'.format(
                        self.command, line, filename)]

        return errors_or_warnings, out_lines


class PyflakesRunner(LintRunner):
    """Run pyflakes, producing flymake readable output.

    The raw output looks like:
      tests/test_richtypes.py:4: 'doom' imported but unused
      tests/test_richtypes.py:33: undefined name 'undefined'
    or:
      tests/test_richtypes.py:40: could not compile
             deth
            ^
    """

    command = 'pyflakes'

    output_matcher = re.compile(
        r'(?P<filename>[^:]+):'
        r'(?P<line_number>[^:]+):'
        r'(?P<description>.+)$')

    @classmethod
    def fixup_data(cls, _line, data):
        # type: (str, Dict[str, str]) -> Dict[str, str]
        if 'imported but unused' in data['description']:
            data['level'] = 'WARNING'
        elif 'redefinition of unused' in data['description']:
            data['level'] = 'WARNING'
        elif 'assigned to but never used' in data['description']:
            data['level'] = 'WARNING'
        elif 'unable to detect undefined names' in data['description']:
            data['level'] = 'WARNING'
        else:
            data['level'] = 'ERROR'
        data['error_type'] = 'PY'
        data['error_number'] = 'F'

        return data


class Flake8Runner(LintRunner):
    """Flake8 has similar output to Pyflakes
    """

    command = 'flake8'

    output_matcher = re.compile(
        r'(?P<filename>[^:]+):'
        '(?P<line_number>[^:]+):'
        '(?P<column_number>[^:]+): '
        '(?P<error_type>[WEFCN])(?P<error_number>[^ ]+) '
        '(?P<description>.+)$')

    @classmethod
    def fixup_data(cls, _line, data):
        # type: (str, Dict[str, str]) -> Dict[str, str]
        if data['error_type'] in ['E']:
            data['level'] = 'WARNING'
        elif data['error_type'] in ['F']:
            data['level'] = 'ERROR'
        else:
            data['level'] = 'WARNING'

        # Unlike pyflakes, flake8 has an error/warning distinction, but some of
        # them are incorrect. Borrow the correct definitions from the pyflakes
        # runner
        if 'imported but unused' in data['description']:
            data['level'] = 'WARNING'
        elif 'redefinition of unused' in data['description']:
            data['level'] = 'WARNING'
        elif 'assigned to but never used' in data['description']:
            data['level'] = 'WARNING'
        elif 'unable to detect undefined names' in data['description']:
            data['level'] = 'WARNING'

        return data

    def get_run_flags(self, _filename):
        # type: (str) -> Tuple[str, ...]
        return (
            '--ignore=' + ','.join(self.ignore_codes),
            # TODO: --select, but additive
            # '-select=' + ','.join(self.enable_codes),
            '--max-line-length', str(self.options.max_line_length),
        )


class Pep8Runner(LintRunner):
    """Run pep8.py, producing flymake readable output.

    The raw output looks like:
      spiders/structs.py:3:80: E501 line too long (80 characters)
      spiders/structs.py:7:1: W291 trailing whitespace
      spiders/structs.py:25:33: W602 deprecated form of raising exception
      spiders/structs.py:51:9: E301 expected 1 blank line, found 0

    """

    command = 'pep8'

    output_matcher = re.compile(
        r'(?P<filename>[^:]+):'
        r'(?P<line_number>[^:]+):'
        r'(?P<column_number>[^:]+):'
        r' (?P<error_number>\w+) '
        r'(?P<description>.+)$')

    @classmethod
    def fixup_data(cls, _line, data):
        # type: (str, Dict[str, str]) -> Dict[str, str]
        data['level'] = 'WARNING'
        return data

    def get_run_flags(self, _filename):
        # type: (str) -> Tuple[str, ...]
        return (
            '--repeat',
            '--ignore=' + ','.join(self.ignore_codes),
            # TODO: make this additive, not a replacement
            # '--select=' + ','.join(self.enable_codes),
            '--max-line-length', str(self.options.max_line_length),
        )


class PylintRunner(LintRunner):
    """ Run pylint, producing flymake readable output.

    The raw output looks like:
    render.py:49: [C0301] Line too long (82/80)
    render.py:1: [C0111] Missing docstring
    render.py:3: [E0611] No name 'Response' in module 'werkzeug'
    render.py:32: [C0111, render] Missing docstring """

    command = 'pylint'

    output_matcher = re.compile(
        r'(?P<filename>[^:]+):'
        r'(?P<line_number>\d+):'
        r'(?P<column_number>\d+):'
        r'\s*\[(?P<error_type>[WECR])(?P<error_number>[^(,\]]+)'
        r'\((?P<symbol>[^)]*)\)'
        r'\s*(?P<context>[^\]]*)\]'
        r'\s*(?P<description>.*)$')

    @classmethod
    def fixup_data(cls, _line, data):
        # type: (str, Dict[str, str]) -> Dict[str, str]
        if data['error_type'].startswith('E'):
            data['level'] = 'ERROR'
        else:
            data['level'] = 'WARNING'

        if data.get('symbol'):
            data['description'] += '  ("{}")'.format(data['symbol'])
        return data

    def get_run_flags(self, _filename):
        # type: (str) -> Tuple[str, ...]
        return (
            '--msg-template', ('{path}:{line}:{column}: '
                               '[{msg_id}({symbol})] {msg}'),
            '--reports', 'n',
            '--disable=' + ','.join(self.ignore_codes),
            # This is additive, not replacing
            '--enable=' + ','.join(self.enable_codes),
            '--dummy-variables-rgx=' + '_.*',
            '--max-line-length', str(self.options.max_line_length),
            '--rcfile', self.options.pylint_rcfile,
        )

    def get_env_vars(self):
        # type: () -> Dict[str, str]
        env = {}
        if self.options.pylint_rcfile:
            env['PYLINTRC'] = self.options.pylint_rcfile
        return env

    def process_returncode(self, returncode):
        # type: (int) -> bool
        # https://docs.pylint.org/en/1.6.0/run.html, pylint returns a bit-encoded exit code.
        return not (returncode & 1 or returncode & 32)


class MyPy2Runner(LintRunner):

    command = 'mypy'

    output_matcher = re.compile(
        r'(?P<filename>[^:]+):'
        r'(?P<line_number>[^:]+):'
        r' (?P<level>[^:]+):'
        r' (?P<description>.+)$')

    _base_flags = [
        '--incremental',
        '--quick-and-dirty',
        '--ignore-missing-imports',
        '--strict-optional',
    ]

    def _get_cache_dir(self, filename):
        # type: (str) -> str
        """Find the appropriate .mypy_cache dir for the given branch.

        We attempt to place the cache directory in the project root,
        under a subdir corresponding to the branch name.
        """
        project_root = find_project_root(filename, self.options.venv_root)
        branch_top = os.path.join(project_root, '.mypy_cache', 'branches')
        # It doesn't make sense to get a branch name unless we actually found a
        # VCS root (i.e. a virtualenv match isn't enough)
        branch = ''                       # type: Optional[str]
        if find_vcs_name(project_root):
            branch = get_vcs_branch_name(project_root)
        if branch:
            cache_dir = os.path.join(branch_top, branch)
        else:
            # Can't figure out current branch, just fake it
            cache_dir = os.path.join(branch_top, 'HEAD')
        return cache_dir

    def get_run_flags(self, filename):
        # type: (str) -> Tuple[str, ...]
        """Determine which mypy (2 or 3) to run, find the cache dir and config file"""
        flags = [
            '--cache-dir={}'.format(self._get_cache_dir(filename)),
        ] + self._base_flags
        if getattr(self.options, 'mypy_config_file', None):
            if not os.path.exists(self.options.mypy_config_file):
                raise FatalException(
                    "Can't find mypy config file %s" % self.options.mypy_config_file,
                    filename)
            # TODO: mypy won't respect per-file config options because our
            # files are named flycheck_<real_file_name>.py at the time of
            # check. We may have to submit a PR on mypy.
            flags += ['--config-file', self.options.mypy_config_file]
        if self.name == 'mypy':
            # mypy2 mode
            flags += ['--py2']
        return tuple(flags)

    def fixup_data(self, _line, data):
        # type: (str, Dict[str, str]) -> Dict[str, str]
        data['level'] = data['level'].upper()
        if data['level'] == 'NOTE':
            return {}
        return data


class MyPy3Runner(MyPy2Runner):

    @property
    def name(self):
        # type: () -> str
        return 'mypy3'


RUNNERS = {
    'pyflakes': PyflakesRunner,
    'flake8': Flake8Runner,
    'pep8': Pep8Runner,
    'pylint': PylintRunner,
    'mypy2': MyPy2Runner,
    'mypy3': MyPy3Runner,
}


def get_options_from_file(file_path):
    # type: (str) -> Dict[str, Any]
    """Parse options from the config file at `file_path` and return them as a dict"""
    parsed_options = {}         # type: Dict[str, Any]

    config = ConfigParser()
    config.read(file_path)
    # [DEFAULT] section
    for key, value in config.defaults().iteritems():
        if is_false(value):
            value = False
        elif is_true(value):
            value = True
        parsed_options[key] = value
    # NOTE: removed support for per-file config file sections, as I don't think
    # they were being used.
    return parsed_options


def update_options_locally(options):
    # type: (Namespace) -> Namespace
    """Merge options from files.

    Traverse the project directory until a config file is found or the
    filesystem root is reached. If found, use overrides from config as
    project-specific settings.
    """
    allowed_duplicate_options = {'extra_ignore_codes'}
    set_options = set()         # type: Set[str]

    dir_path = os.path.dirname(os.path.abspath(options.file))
    config_file_path = os.path.join(dir_path, CONFIG_FILE_NAME)
    while True:
        if os.path.exists(config_file_path):
            new_options = get_options_from_file(config_file_path)
            for key, value in new_options.items():
                if key in set_options and key not in allowed_duplicate_options:
                    # Already set this option from a file, don't set it again
                    continue
                set_options.add(key)
                # Special handling for some keys

                # Special case config files to contain the full path - assume
                # the specified path is absolute, or relative to the current
                # .pycheckers file
                if 'config_file' in key:
                    if not os.path.isabs(value):
                        value = os.path.join(os.path.dirname(config_file_path), value)
                # Allow for extending, rather than replacing, ignore codes
                elif key == 'extra_ignore_codes':
                    # Still a comma-separated str
                    value = ','.join([options.ignore_codes, value])
                    key = 'ignore_codes'
                setattr(options, key, value)

            if not options.merge_configs:
                # We don't want to walk further up looking for config files
                break

        # Walk up a directory and try again for another file
        parent = os.path.dirname(dir_path)
        if parent == dir_path:
            break
        dir_path = parent
        config_file_path = os.path.join(dir_path, CONFIG_FILE_NAME)
    return options


def run_one_checker(ignore_codes, enable_codes, options, source_file, checker_name):
    # type: (Tuple[str], Tuple[str], Namespace, str, str) -> Tuple[int, List[str]]
    checker_class = RUNNERS[checker_name]
    runner = checker_class(ignore_codes, enable_codes, options)
    errors_or_warnings, out_lines = runner.run(source_file)
    return (errors_or_warnings, out_lines)


def find_vcs_name(dir_):
    # type: (str) -> Optional[str]
    """If dir_ is a VCS root, return the name of the VCS, otherwise None"""
    for part in ['.git', '.svn', '.hg', '.cvs', '.jedi']:
        path = os.path.join(dir_, part)
        if os.path.exists(path) and os.path.isdir(path):
            return part[1:]             # return the name of the vcs system
    return None


def find_vcs_root(source_file):
    # type: (str) -> Tuple[Optional[str], Optional[str]]
    """Returns the path to the root and the name of the VCS system, if found"""
    cur_dir = os.path.dirname(source_file)
    while True:
        vcs_name = find_vcs_name(cur_dir)
        if vcs_name:
            return cur_dir, vcs_name
        parent = os.path.dirname(cur_dir)
        if parent == cur_dir:
            break              # Hit the FS root without finding VCS info
        cur_dir = parent
    return None, None


def get_vcs_branch_name(vcs_root):
    # type: (str) -> Optional[str]
    """If under source control and the VCS supports branches, find branch name.
    """
    # TODO: only supports git for now
    commands = {
        'git': ['git', 'symbolic-ref', '--short', 'HEAD'],
    }
    vcs_name = find_vcs_name(vcs_root)
    if not vcs_name or vcs_name not in commands:
        # Unsupported VCS
        return None

    args = commands[vcs_name]
    p = Popen(
        args, stdout=PIPE, stderr=PIPE, cwd=vcs_root, universal_newlines=True)
    out, _err = p.communicate()
    p.wait()
    out = out.strip()
    return out if out else None


def guess_virtualenv(source_file, venv_root):
    # type: (str, str) -> Tuple[Optional[str], Optional[str]]
    """Return the paths to the project root and the virtualenv that
    corresponds to this source file, if any.

    The virtualenv name must match the name of one of the containing
    directories.
    """
    full_path = os.path.abspath(source_file)
    dir_components = os.path.dirname(full_path).split(os.sep)
    virtualenv_base = os.path.expanduser(venv_root)
    used_components = [os.sep]
    for component in dir_components:
        if not component:
            continue
        used_components.append(component)
        virtualenv_path = os.path.join(virtualenv_base, component)
        if os.path.exists(virtualenv_path):
            return os.path.join(*used_components), virtualenv_path
    return None, None


def set_path_for_virtualenv(source_file, venv_root):
    # type: (str, str) -> None
    """Determine if the current file is part of a package that has a
    virtualenv, and munge paths appropriately"""

    _project_root, venv_path = guess_virtualenv(source_file, venv_root)
    if venv_path:
        bin_path = os.path.join(venv_path, 'bin')
        os.environ['PATH'] = bin_path + ':' + os.environ['PATH']


def find_project_root(source_file, venv_root):
    # type: (str, str) -> str
    """Find the root directory of the current project.

    1. Walk up the directory tree looking for a VCS directory.
    2. Failing that, find a virtualenv that matches a part of the
           directory, and choose that as the root.
    3. Otherwise, just use the local directory.
    """
    # Case 1
    vcs_root, _vcs_name = find_vcs_root(source_file)
    if vcs_root:
        return vcs_root

    # Case 2
    project_dir, _venv_path = guess_virtualenv(source_file, venv_root)
    if project_dir:
        return project_dir

    # Case 3
    return os.path.dirname(source_file)


def parse_args():
    # type: () -> Namespace

    parser = ArgumentParser()
    parser.add_argument('file', type=str, help='Filename to check')
    parser.add_argument("-c", "--checkers", dest="checkers",
                        default=default_checkers,
                        help="Comma-separated list of checkers")
    parser.add_argument("-i", "--ignore-codes", dest="ignore_codes",
                        default='',
                        help="Comma-separated list of error codes to ignore")
    parser.add_argument("-e", "--enable-codes", dest="enable_codes",
                        default='',
                        help="Comma-separated list of error codes to ignore")
    parser.add_argument('--max-line-length', dest='max_line_length',
                        default=80, action='store',
                        help='Maximum line length')
    parser.add_argument('--no-merge-configs', dest='merge_configs',
                        action='store_false',
                        help=('Whether to ignore config files found at a '
                              'higher directory than this one'))
    parser.add_argument('--multi-thread', type=str2bool, default=True,
                        action='store',
                        help=('Run checkers sequentially, '
                              'rather than simultaneously'))
    parser.add_argument('--venv-root', dest='venv_root',
                        default='~/.virtualenvs', action='store',
                        help='Location of Python virtual environments')
    parser.add_argument('--pylint-rcfile', default='.pylintrc',
                        dest='pylint_rcfile',
                        help='Location of a config file for pylint')
    parser.add_argument('--mypy-config-file', default=None,
                        dest='mypy_config_file',
                        help='Location of a config file for mypy')
    parser.add_argument('--report-checker-errors-inline', type=str2bool, default=True,
                        action='store',
                        help=("Whether to fake failing checkers's STDERR as a reported "
                              "error for easier display."))

    return parser.parse_args()


def main():
    # transparently add a virtualenv to the path when launched with a venv'd
    # python. We can sometimes count on emacs to launch us with the correct
    # python, but we need to handle being run manually, or with emacs in a
    # confused state.
    os.environ['PATH'] = (os.path.dirname(sys.executable) + ':' +
                          os.environ['PATH'])

    options = parse_args()

    source_file = options.file
    if not os.path.exists(source_file):
        raise RuntimeError("Can't find source file %s" % source_file)

    options = update_options_locally(options)

    checkers = options.checkers
    ignore_codes = tuple(c for c in options.ignore_codes.split(",") if c)
    enable_codes = tuple(c for c in options.enable_codes.split(",") if c)
    set_path_for_virtualenv(source_file, options.venv_root)

    checker_names = [checker.strip() for checker in checkers.split(',')]
    try:
        [RUNNERS[checker_name] for checker_name in checker_names]
    except KeyError:
        croak(("Unknown checker {}".format(checker_name),  # pylint: disable=used-before-assignment
               "Expected one of %s" % ', '.join(RUNNERS.keys())),
              filename=options.file)

    if options.multi_thread:
        from multiprocessing import Pool, cpu_count
        p = Pool(cpu_count() + 1)

        func = partial(
            run_one_checker, ignore_codes, enable_codes, options, source_file)

        outputs = p.map(func, checker_names)
        p.close()
        p.join()
        counts, out_lines_list = zip(*outputs)
        errors_or_warnings = sum(counts)
    else:
        errors_or_warnings = 0
        out_lines_list = []
        for checker_name in checker_names:
            e_or_w, o_l = run_one_checker(
                ignore_codes, enable_codes, options, source_file, checker_name)
            errors_or_warnings += e_or_w
            out_lines_list.append(o_l)

    for out_lines in out_lines_list:
        for line in out_lines:
            print(line)

    sys.exit(errors_or_warnings > 0)


if __name__ == '__main__':
    main()
