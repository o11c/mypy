"""Utilities for verifying git integrity."""

# Used also from setup.py, so don't pull in anything additional here (like mypy or typing):
import os
import pipes
import subprocess
import sys


def is_git_repo(dir: str) -> bool:
    """Is the given directory version-controlled with git?"""
    return os.path.exists(os.path.join(dir, ".git"))


def have_git() -> bool:
    """Can we run the git executable?"""
    try:
        subprocess.check_output(["git", "--help"])
        return True
    except subprocess.CalledProcessError:
        return False
    except OSError:
        return False


def get_submodules(dir: str):
    """Return a list of all git top-level submodules in a given directory."""
    # It would be nicer to do
    # "git submodule foreach 'echo MODULE $name $path $sha1 $toplevel'"
    # but that wouldn't work on Windows.
    output = subprocess.check_output(["git", "submodule", "status"], cwd=dir)
    # "<status><sha1> name desc"
    # status='-': not initialized
    # status='+': changed
    # status='u': merge conflicts
    # status=' ': up-to-date
    for line in output.splitlines():
        # Skip the status indicator, as it could be a space can confuse the split.
        line = line[1:]
        name = line.split(b" ")[1]
        yield name.decode(sys.getfilesystemencoding())


def git_revision(dir: str) -> bytes:
    """Get the SHA-1 of the HEAD of a git repository."""
    return subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=dir).strip()


def submodule_revision(dir: str, submodule: str) -> bytes:
    """Get the SHA-1 a submodule is supposed to have."""
    output = subprocess.check_output(["git", "ls-files", "-s", submodule], cwd=dir).strip()
    # E.g.: "160000 e4a7edb949e0b920b16f61aeeb19fc3d328f3012 0       typeshed"
    return output.split()[1]


def head_contains(dir: str, expected: str) -> bool:
    """Check if a git repository has a strictly newer commit."""
    output = subprocess.call(["git", "merge-base", "--is-ancestor", expected, "HEAD"], cwd=dir)
    return output == 0


def is_dirty_cache(dir: str) -> bool:
    """Check whether a git repository has uncommitted changes in the index."""
    output = subprocess.check_output(["git", "diff", "--cached", "--name-only"], cwd=dir)
    return output.strip() != b""


def is_dirty_working(dir: str) -> bool:
    """Check whether a git repository has unstaged changes in the working tree."""
    output = subprocess.check_output(["git", "diff", "--name-only"], cwd=dir)
    return output.strip() != b""


def has_extra_files(dir: str) -> bool:
    """Check whether a git repository has untracked files."""
    output = subprocess.check_output(["git", "clean", "--dry-run", "-d"], cwd=dir)
    return output.strip() != b""


def warn_no_git_executable() -> None:
    print("Warning: Couldn't check git integrity. "
          "git executable not in path.", file=sys.stderr)


def warn_dirty_cache(dir) -> None:
    print("Warning: git module '{}' has uncommitted changes.".format(dir),
          file=sys.stderr)
    print("Remember to go to the directory", file=sys.stderr)
    print("  {}".format(dir), file=sys.stderr)
    print("and commit your changes", file=sys.stderr)


def warn_dirty_working(dir) -> None:
    print("Warning: git module '{}' has unstaged changes.".format(dir),
          file=sys.stderr)
    print("Remember to go to the directory", file=sys.stderr)
    print("  {}".format(dir), file=sys.stderr)
    print("and add your changes", file=sys.stderr)


def warn_extra_files(dir) -> None:
    print("Warning: git module '{}' has untracked files.".format(dir),
          file=sys.stderr)
    print("Remember to go to the directory", file=sys.stderr)
    print("  {}".format(dir), file=sys.stderr)
    print("and add or ignore your new files.", file=sys.stderr)


def chdir_prefix(dir) -> str:
    """Return the command to change to the target directory, plus '&&'."""
    if os.path.relpath(dir) != ".":
        return "cd " + pipes.quote(dir) + " && "
    else:
        return ""


def error_submodule_not_initialized(name: str, dir: str) -> None:
    print("Error: Submodule '{}' not initialized.".format(name), file=sys.stderr)
    print("Please run:", file=sys.stderr)
    print("  {}git submodule update --init {}".format(
        chdir_prefix(dir), name), file=sys.stderr)


def warning_submodule_newer(name: str, dir: str) -> None:
    print("Warning: Submodule '{}' has new commits.".format(name))
    print("Remember to run:", file=sys.stderr)
    print("  {}git add {}".format(
        chdir_prefix(dir), name), file=sys.stderr)


def error_submodule_not_updated(name: str, dir: str) -> None:
    print("Error: Submodule '{}' not updated.".format(name), file=sys.stderr)
    print("Please run:", file=sys.stderr)
    print("  {}git submodule update {}".format(
        chdir_prefix(dir), name), file=sys.stderr)


def verify_git_integrity_or_abort(datadir: str) -> None:
    """Verify the (submodule) integrity of a git repository.

    Potentially output warnings/errors (to stderr), and exit with status 1
    if we detected a severe problem.
    """
    datadir = datadir or '.'
    if not is_git_repo(datadir):
        return
    if not have_git():
        warn_no_git_executable()
        return
    for submodule in get_submodules(datadir):
        submodule_path = os.path.join(datadir, submodule)
        if not is_git_repo(submodule_path):
            error_submodule_not_initialized(submodule, datadir)
            sys.exit(1)
        expected = submodule_revision(datadir, submodule)
        actual = git_revision(submodule_path)
        if expected != actual:
            if head_contains(submodule_path, expected.decode("ascii")):
                warning_submodule_newer(submodule, datadir)
            else:
                error_submodule_not_updated(submodule, datadir)
                sys.exit(1)
        if is_dirty_cache(submodule_path):
            warn_dirty_cache(submodule)
        if is_dirty_working(submodule_path):
            warn_dirty_working(submodule)
        if has_extra_files(submodule_path):
            warn_extra_files(submodule)
