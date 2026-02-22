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

A countdown timer appears at the top of the screen in green. When time expires it turns red and displays "Time's up!"
The session keeps running so you can finish your thought. It's more of a way to help reach a minimum amount
of time writing. Press `Esc` to quit as usual.

## Releasing a new version

1. Bump `version` in `package.yaml` (e.g. `0.2.0.0`) and merge all changes to `main`.
2. On GitHub, go to **Releases → Draft a new release**.
3. Create a new tag (e.g. `v0.2.0`) targeting `main`.
4. Write release notes, then click **Publish release**.

The [Release workflow](.github/workflows/release.yml) will automatically build the Linux binary
and attach `tape-linux-x86_64.tar.gz` to the release. First run may take longer (30–45 minutes) due to cache warming;
subsequent releases are should hopefully be faster.

## Feature ideas

### Visual indication of progress toward word goal
Have the words that go off the screen fill up some kind of container
so that users can see how much progress they have made toward a goal of
writing a certain amount of words

### Daily freewrite management
What does it look like to use this tool to do a daily free-writing session?

### Persist tapes between sessions
In freewriting what data would you want to persist between sessions?

### Export for org mode and markdown
Make it so that you can save your daily sessions to other formats and
be able to view them that way.

### Tapes return to the beginning after fixed number of characters so you can review what you

### Decoration of the Deck
The tape are in a tape deck. Add some unicode to make it look like that.
Show a little picture of a turning cassette so the user can see where to go next or where they are in the tape.
