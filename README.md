# tape

Freewrite and flow write with the tape TUI


https://github.com/user-attachments/assets/99ae9710-b653-471b-9224-d3db31e22e79


## Getting started

Install with
```
stack install
```

Once installed launch with `tape`.

## Timed sessions

Start a timed session with the `-t` flag followed by the number of minutes:

```
tape -t 10
```

A countdown timer appears at the top of the screen in green. When time expires it turns red and displays "Time's up!" — but the session keeps running so you can finish your thought. Press `Esc` to quit as usual.

Invalid or zero values for `-t` are silently ignored and tape starts without a timer.

## Releasing a new version

1. Bump `version` in `package.yaml` (e.g. `0.2.0.0`) and merge all changes to `main`. Ensure tests pass (`stack test`).
2. On GitHub, go to **Releases → Draft a new release**.
3. Create a new tag (e.g. `v0.2.0`) targeting `main`.
4. Write release notes, then click **Publish release**.

The [Release workflow](.github/workflows/release.yml) will automatically build the Linux binary and attach `tape-linux-x86_64.tar.gz` to the release. First run takes 30–45 minutes due to cache warming; subsequent releases are faster.

## Feature ideas

### Timed topics
After a certain number of seconds, a prompt appears to the right of the fixed center cursor.
The prompt can then be overwritten by the new typing. This infuses new ideas into the flow state.

### Select number of tapes on start up

### Persist tapes between sessions

### Tapes return to the beginning after fixed number of characters so you can review what you

### Decoration of the Deck
The tape are in a tape deck. Add some unicode to make it look like that.
Show a little picture of a turning cassette so the user can see where to go next or where they are in the tape.
