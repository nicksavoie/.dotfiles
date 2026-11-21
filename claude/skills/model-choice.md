# model-choice

The default model suits build and routine work. Design work (a project spec, an ownership or schema decision, a multi-file plan) is where a stronger model earns its cost, and the person has to switch with `/model`; the assistant cannot.

## The rule

```markdown
When a Spec phase starts and the session is not already on the stronger model, say one line: "Spec phase: `/model opus` is worth trying; switch back for the build." At most once per session, never during Build, never again after the user declines.
```

Keep the default in settings and set the effort level per model, so a swap does not silently change effort (`settings.md`).

## Measure

Count assistant messages per model in a session transcript:

```
jq -r 'select(.type=="assistant") | .message.model' <transcript>.jsonl | sort | uniq -c
```

Use assistant messages only; a plain text search for `model` matches other fields.
