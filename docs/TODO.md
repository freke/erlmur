# erlmur Development TODO List

This document outlines the incremental steps required to align the existing codebase with the final architecture defined
in the design documents. Each item is intended to be a small, manageable change.

## Phase 1: Refactoring & Core Feature Updates

This phase focuses on restructuring the existing code to match the design and updating the core data models. This will
provide a solid foundation for adding new features.

- [ ] **1. Create the Message Handler Module:**
  - **File:** `src/erlmur_message_handler.erl` (New file)
  - **Task:** Create the new module with an exported `handle/2` function. This will serve as the central point for
    processing all Mumble protocol messages.

- [ ] **2. Delegate Message Handling from Session:**
  - **File:** `src/erlmur_session.erl`
  - **Task:** In the `handle_info({ssl, ...})` clause, change the call from `erlmur_tcp_handler:handle(Msg, ...)` to
    `erlmur_message_handler:handle(Msg, ...)`. This officially delegates the responsibility of message processing.

- [ ] **3. Move Logic to Message Handler:**
  - **File:** `src/erlmur_message_handler.erl`
  - **Task:** Move the implementation of the large `handle/2` function from `erlmur_tcp_handler.erl` into this module.
    The function in `erlmur_tcp_handler.erl` can now be removed.

- [ ] **4. Update User Data Model:**
  - **File:** `include/erlmur.hrl`
  - **Task:** Update the `#user{}` record definition to include the fields from the data model, such as `groups` and
    `comment_hash`.

- [ ] **5. Enhance User State Management:**
  - **File:** `src/erlmur_users.erl`
  - **Task:** Refactor the `update/3` function to be more robust. It should handle the new fields in the `#user{}`
    record and log warnings for unknown fields, rather than using simple pattern matching.

- [ ] **6. Update Channel Data Model:**
  - **File:** `include/erlmur.hrl`
  - **Task:** Update the `#channel{}` record definition to include the `acls` field.

- [ ] **7. Enhance Channel State Management:**
  - **File:** `src/erlmur_channels.erl`
  - **Task:** Modify the `add/1` and `update/2` functions to correctly handle the new `acls` field when creating or
    modifying channels.

## Phase 2: New Feature Implementation

With the refactoring complete, we can now build the missing features from the design.

- [ ] **1. Define Ban Data Structure:**
  - **File:** `include/erlmur.hrl`
  - **Task:** Define the `#ban{}` record based on the `BAN` entity in the data model.

- [ ] **2. Create Ban Management Module:**
  - **File:** `src/erlmur_ban.erl` (New file)
  - **Task:** Create the new module to manage the ban list. It should include functions like `init/1`, `add/1`,
    `is_banned/1`, and use Mnesia for persistence.

- [ ] **3. Supervise Ban Module:**
  - **File:** `src/erlmur_sup.erl`
  - **Task:** Add the `erlmur_ban` worker to the main supervisor's child list.

- [ ] **4. Enforce Bans on Connection:**
  - **File:** `src/erlmur_message_handler.erl`
  - **Task:** In the `handle/2` clause that processes the `Authenticate` message, add a call to
    `erlmur_ban:is_banned/1` to reject banned users before authentication.

- [ ] **5. Create ACL Engine Module:**
  - **File:** `src/erlmur_acl.erl` (New file)
  - **Task:** Create the stateless ACL engine module.
    Start with a placeholder `query_permissions/2` function that returns `allow` by default.

- [ ] **6. Integrate ACL Checks:**
  - **File:** `src/erlmur_message_handler.erl`
  - **Task:** For a single privileged action (e.g., handling a `UserState` message that mutes another user), add a call
    to `erlmur_acl:query_permissions/2` before processing the request.

- [ ] **7. Implement ACL Logic:**
  - **File:** `src/erlmur_acl.erl`
  - **Task:** Fully implement the permission resolution logic inside the `query_permissions/2` function. This involves reading ACL rules
    from `erlmur_channels`, user groups from `erlmur_users`, and applying the Mumble inheritance rules.

- [ ] **8. Build Public API Facade:**
  - **File:** `src/erlmur.erl`
  - **Task:** Implement the public API functions as defined in the design (e.g., `get_users/0`, `get_channels/0`,
    `kick_user/2`). These functions will delegate calls to the appropriate internal modules.
