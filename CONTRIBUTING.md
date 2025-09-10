# Contributing to erlmur

First off, thank you for considering contributing to erlmur! Any contribution, from reporting a bug to implementing a
new feature, is greatly appreciated.

This document provides a set of guidelines for contributing to this project.

## How Can I Contribute?

### Reporting Bugs

If you find a bug, please ensure the bug was not already reported by searching on GitHub under
[Issues](https://github.com/your-repo/erlmur/issues).

If you're unable to find an open issue addressing the problem, [open a new one](https://github.com/your-repo/erlmur/issues/new).
Be sure to include a **title and clear description**, as much relevant information as possible, and a **code sample** or
an **executable test case** demonstrating the expected behavior that is not occurring.

### Suggesting Enhancements

If you have an idea for an enhancement, please open an issue to discuss it. This allows us to coordinate our efforts
and prevent duplication of work.

Before creating enhancement suggestions, please check the [issue list](https://github.com/your-repo/erlmur/issues) to see
if the enhancement has already been suggested.

## Development Process

1. **Adhere to the Design:** All contributions must follow the architectural and data model designs laid out in the
    `/docs` directory. If your contribution requires a change to the design, please open an issue to discuss the
    proposed changes first.
2. **Fork the repository** and create your branch from `main`.
3. **Set up your environment:** Use `just shell` to enter the development environment.
4. **Make your changes:** Ensure you adhere to the existing code style and conventions.
5. **Add tests:** All new features or bug fixes must be accompanied by corresponding tests. Run the full test suite
    with `just test` to ensure nothing has broken.
6. **Commit your changes:** Follow the existing commit message style.
7. **Create a Pull Request:** Push your branch to your fork and open a pull request to the `main` branch of the
    original repository.

Thank you for your contribution!
