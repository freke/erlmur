# Erlmur

`erlmur` is a Mumble server implementation written in Erlang/OTP. Mumble is a
low-latency, high-quality voice chat application, and this project aims to provide a
robust and scalable server for it.

## Features

* **Voice Chat:** Core functionality for voice communication.
* **Text Messaging:** Support for text messages between clients.
* **Channel Management:** Hierarchical channel structure for organizing users.
* **User Management:** Basic user authentication and state management.

## Getting Started

### Prerequisites

* Erlang/OTP 25 or later
* `rebar3`
* `just`

For a more streamlined setup, this project provides a reproducible development
environment using [Nix](https://nixos.org/) and [devenv.sh](https://devenv.sh/).
If you have these tools installed, you can enter a shell with all the required
dependencies by running:

```sh
devenv shell
```

### Installation

1. **Clone the repository:**

    ```sh
    git clone https://github.com/your-repo/erlmur.git
    cd erlmur
    ```

2. **Build the project:**

    ```sh
    just build
    ```

### SSL Certificate

For the server to accept secure connections, you need an SSL certificate. By
default, the server looks for `priv/cert.pem` and `priv/key.pem`. You can
generate a self-signed certificate for development purposes:

```sh
mkdir -p priv
openssl genrsa -out priv/key.pem 2048
openssl req -new -x509 -key priv/key.pem -out priv/cert.pem -days 365
```

Running `just ct` or `just shell` will generate test certificates for development.

You can also configure the paths to your certificate and key files in your
`rebar.config` or via environment variables. See the `erlmur.app.src` file for
more details.

### Running the Server

Once the project is built and the SSL certificate is in place, you can start the
server by running:

```sh
just shell
```

This will start an Erlang shell with the application running.

## Development

This project uses `just` as a command runner to simplify common development
tasks.

* **Run the test suite:**

    ```sh
    just test
    ```

* **Format the code:**

    ```sh
    just format
    ```

* **Generate documentation:**

    ```sh
    just doc
    ```

## Contributing

Contributions are welcome! Please see the [CONTRIBUTING.md](CONTRIBUTING.md) file
for guidelines on how to contribute to this project.
