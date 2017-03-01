# Week 2 Exercises

See [Root README for more info](../README.md)

Read error.log in order:

```
intercalate "\n" . (map (\m -> case m of {(Unknown u) -> u; (LogMessage _ _ m) -> m})) . parse <$> readFile "error.log"
```