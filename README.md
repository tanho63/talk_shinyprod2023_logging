# Effective Logging in Shiny

A talk given at Shiny in Production 2023 about effective logging strategies for 
production Shiny apps, at both an application-logging level as well as for production
systems.


Slides: [GoogLe Slides](https://docs.google.com/presentation/d/1xt7rEwlkm8Uwtu1DWrrJ9n08V_V77Fxf8aKREPlVzrg/edit), [PDF](https://github.com/tanho63/talk_shinyprod2023_logging/blob/main/effective_logging_for_shiny.pdf)

Video: TBD

Code examples and snippets live in the [`examples/` directory](https://github.com/tanho63/talk_shinyprod2023_logging/tree/main/examples)
of this repository. This includes:

- a `sitrep()` function I really like as an improved `session_info()`
- an example of adding an error handler on fatal app crashes
