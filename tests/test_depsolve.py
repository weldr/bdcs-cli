#pylint: disable=missing-docstring,import-error,too-many-public-methods

import os
import unittest
import warnings
from subprocess import check_output, CalledProcessError

# silence resource warnings while testing, see
# https://emptysqua.re/blog/against-resourcewarnings-in-python-3/
warnings.simplefilter("ignore", ResourceWarning)

import toml

try:
    from parameterized import parameterized
except ImportError:
    # on Fedora 25 (Docker)
    from nose_parameterized import parameterized


_TEST_DIR = os.path.dirname(__file__)

_RECIPE_DIR = os.path.join(_TEST_DIR, '..', 'examples', 'recipes')
_RECIPE_DIR = os.path.abspath(_RECIPE_DIR)

_DEPSOLVE = os.path.join(_TEST_DIR, '..', 'dist', 'build', 'bdcs-cli', 'bdcs-cli')
_DEPSOLVE = [os.path.abspath(_DEPSOLVE)]
if os.environ.get('START_API_EXTERNALLY'):
    _DEPSOLVE.extend(['--url', 'http://api:4000/'])
_DEPSOLVE.extend(['recipes', 'depsolve'])

_RECIPES = []
for _dirpath, _dirnames, filenames in os.walk(_RECIPE_DIR):
    for _recipe in filenames:
        if _recipe.endswith('.toml'):
            _RECIPES.append(_recipe)


def read_toml(fname):
    with open(os.path.join(_RECIPE_DIR, fname), 'r') as f_handle:
        return toml.loads(f_handle.read())

def exec_depsolve(recipe_name):
    # will raise CalledProcessError exception if depsolve
    # exit status is != 0. Returns string
    return check_output(_DEPSOLVE + [recipe_name]).decode('utf-8')


class DepsolveTestCase(unittest.TestCase):
    @parameterized.expand(_RECIPES)
    def test_depsolve(self, recipe_file):
        recipe = read_toml(recipe_file)

        expected_packages = []
        for key in ['packages', 'modules']:
            if key in recipe:
                for pkg in recipe[key]:
                    expected_packages.append(pkg['name'])

        # step 1: can we resolve all the dependencies
        try:
            recipe_name = recipe_file.replace('.toml', '')
            depsolve_output = exec_depsolve(recipe_name)
            # remove the first line which is something like:
            # Recipe: glusterfs v0.0.1\n
            # ... then followed by a list of RPM NEVRA
            # we don't want to accidentally match a package name against
            # the recipe name when they exist (e.g. atlas)
            depsolve_output = depsolve_output.strip().split('\n')[1:]

            # then turn back the list into a string to make
            # assertions below easier
            depsolve_output = '\n'.join(depsolve_output)
        except CalledProcessError as err:
            self.assertEqual('', err.output)
            self.fail('depsolve failed!')

        # step 2: is what the user wanted still in the list
        for pkg in expected_packages:
            self.assertIn(pkg, depsolve_output)

        # step 3: sanity test specific functionality
        # this is now https://github.com/weldr/bdcs-cli/pull/16

if __name__ == "__main__":
    unittest.main()
