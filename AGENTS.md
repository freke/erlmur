# AGENTS.md - erlmur Development Guide

## Build Commands

```bash
just build      # Format, compile, and build release
just dev       # Start development shell
just test      # Run all tests (xref, dialyzer, proper, eunit, ct)
just eunit     # Run eunit tests
just ct         # Run common test suite
just proper    # Run property-based tests (1000 iterations)
just dialyzer  # Static analysis
just xref      # Cross-reference analysis
just format    # Format code with erlfmt
just clean     # Clean build artifacts
```

## Code Style Guidelines

### Imports and Includes
- Use `-include("module.hrl")` for local records
- Use `-include_lib("app/include/module.hrl")` for other app headers
- Include gpb headers last: `-include("Mumble_gpb.hrl")`

### Module Documentation
- All modules must have `-moduledoc` with description
- Export functions must have `-doc` with input/output specs
- Use `-spec` for all exported functions with proper types
- Callback functions need `-doc` in behaviour definitions

### Records and State
- Use records for process state (`-record(state, {...})`)
- Use maps for protocol messages
- Access record fields via `#state.field` or `#state{field = Value}`
- Define opaque types with `-opaque` and `-export_type`

### Naming Conventions
- Modules: `snake_case.erl`
- Functions: `snake_case`
- Records: `snake_case`
- Constants: `UPPER_SNAKE`
- Variables: `UpperCamelCase`
- Message types: `'CamelCase'`

### Function Structure
```erlang
-spec function_name(arg1(), arg2()) -> return_type().
function_name(Arg1, Arg2) ->
    case do_something(Arg1) of
        {ok, Result} ->
            handle_result(Result, Arg2);
        {error, Reason} ->
            handle_error(Reason)
    end.
```

### Guards and Pattern Matching
- Use guards for input validation
- Pattern match in function heads when possible
- Use `when` guards for type/size checks
- Prefer specific patterns over catch-all clauses

### Error Handling
- Return `{ok, Value}` or `{error, Reason}` for recoverable errors
- Use `try...of...else` for exceptions that need cleanup
- Log errors with appropriate level (`logger:error`, `logger:warning`)
- Match specific errors before general ones

### gen_statem Conventions
- Use named state functions (`state_name/3`)
- Export callback mode as list: `callback_mode() -> [state_functions, state_enter]`
- Handle `enter` events for state transitions
- Return `{keep_state, Data}` or `{next_state, Name, Data}`
- Use `?TIMEOUT` constant for timeouts

### Logging Guidelines

Log levels should be used consistently across the codebase:

**ERROR** - Failures requiring immediate attention:
- Connection failures, startup failures, send failures
- MAC mismatches in crypto operations
- Critical errors that prevent normal operation

**WARNING** - Unexpected but handled situations:
- Unhandled messages in state machines
- Invalid authentication attempts
- Timeouts and connection losses
- UDP decrypt failures

**NOTICE** - Significant system events (not routine operations):
- Application fully started/stopped
- Server listening on port
- Session established
- UDP verified (once, not repeatedly)
- Certificate generation
- Major state transitions

**INFO** - Progress indicators and authentication events:
- User authenticated/disconnected
- Server/component started (milestones only)
- High-level operational status
- Use sparingly - only for major progress events

**DEBUG** - Detailed troubleshooting information:
- Individual message receipts/transmissions
- Protocol encoding/decoding details
- Startup step-by-step progress
- Voice data receipt (indicate only, no content)
- Internal state transitions
- Performance metrics

**Never log sensitive data** (passwords, keys, message content) at INFO or higher levels.

Always include context in log messages:
```erlang
logger:error("Failed to start server on port ~p: ~p", [Port, Reason])
logger:notice("Session ~p established for user ~s", [SessionId, Username])
```

### Testing
- EUnit: Use `?_test(Fun)` and `?_assert*` macros
- Common Test: Define `-suite()` and `-export([suite/0, ...])`
- Proper: Define properties with `prop_*` and `?FORALL`
- Mocks: Use Meck for OTP module mocking

### Protocol Messages
- Messages are maps with `message_type => 'MessageName'`
- Use atoms for message types (matching GPB definitions)
- Validate required fields before processing
- Include version information in handshake

### Log Level Configuration

The log level can be configured via:

1. **Environment variable** (highest priority):
   ```bash
   ERLMUR_LOG_LEVEL=info rebar3 shell
   ERLMUR_LOG_LEVEL=notice rebar3 shell
   ERLMUR_LOG_LEVEL=debug rebar3 shell
   ```

2. **Application environment** (in sys.config):
   ```erlang
   [{erlmur, [{log_level, info}]}].
   ```

3. **Default**: `info` (shows all `logger:info` messages)

Available levels: `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency`
