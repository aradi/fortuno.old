|Fortuno logo|

**************************************
Fortuno – Fortran Unit Testing Objects
**************************************

**NOTE**: The Fortuno repository had been relocated and this repository is going to be deleted soon.
Visit the project in the `new Fortuno repository <https://github.com/fortuno-repos/fortuno>`_.

The **Fortuno** project offers an object oriented unit testing framework for the
Fortran language. It aims to combine the simplicity of `test-drive
<https://github.com/fortran-lang/test-drive>`_ with the broad range of features
offered by `pFUnit <https://github.com/Goddard-Fortran-Ecosystem/pFUnit>`_ and
to extend upon their capabilities. It is written in Fortran 2018 and can be
directly used in Fortran projects without a need for a special pre-processor.

**Fortuno** provides

- serial unit testing,

- parallel unit testing for MPI- and coarray-parallel projects,

- fixtured tests,

- parameterized tests

- seamless integration with the CMake systems (fpm & meson in progress)

Detailed documentation will be soon found on the `Fortuno documentation page
<https://fortuno.readthedocs.io>`_ on Read the Docs. Currently, have a look
at some of the example files in the `test folder <test/regression/>`_.

The development can be followed on the `Fortuno project page
<https://github.com/aradi/fortuno>`_  on GitHub.


Known issues
============

In order to be flexible, expressive and simple, Fortuno uses modern Fortran
constructs quite extensively. Unfortunately, this seems to be challenging for
some compilers, as shown in the compiler table below.

+------------------------+-----------------------------------------------------+
| Compiler               | Status                                              |
+========================+=====================================================+
| Intel 2021.7           | * serial: OK                                        |
| (x86_64/Linux)         | * mpi (with Intel MPI): OK                          |
|                        | * coarray: OK                                       |
+------------------------+-----------------------------------------------------+
| NAG 7.1 (build 7111,   | * serial: OK (Release mode only)                    |
| x86_64/Linux)          | * coarray: OK (Release mode only)                   |
+------------------------+-----------------------------------------------------+
| GNU 12.2               | * serial: Internal compiler error (failureinfo.f90) |
| (x86_64/Linux)         |                                                     |
+------------------------+-----------------------------------------------------+

If you are aware of other compilers being able to build Fortuno, open an issue
or a pull request, so that the table can be updated accordingly.


License
=======

This project is licensed under the `BSD-2-Clause Plus Patent License
<https://opensource.org/licenses/BSDplusPatent>`_. The SPDX license identifier
for this project is `BSD-2-Clause-Patent
<https://spdx.org/licenses/BSD-2-Clause-Patent.html>`_.


..  |Fortuno logo| image:: assets/fortuno-128.png
    :alt: Fortuno unit testing framework
    :target: https://fortuno.readthedocs.io
