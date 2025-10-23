# GUI Programming & Paint Application

A functional GUI library and paint application built in OCaml, featuring a custom widget system, event handling, and interactive drawing tools.

## Overview

This project implements a complete GUI framework from scratch, including:

- **Custom Widget System** - Modular, composable UI components
- **Event-Driven Architecture** - Sophisticated event loop and handlers
- **Paint Application** - Full-featured drawing program with multiple tools
- **Graphics Context** - Abstraction layer for rendering operations
- **Data Structures** - Efficient deque implementation for undo/redo functionality

## Project Structure

```
.
├── widget.ml/mli       # Core widget system and composition
├── gctx.ml/mli         # Graphics context for drawing operations
├── paint.ml/mli        # Main paint application
├── eventloop.ml/mli    # Event handling and dispatching
├── deque.ml/mli        # Double-ended queue data structure
├── assert.ml/mli       # Testing utilities
└── widgetTest.ml       # Widget system tests
```

## Demo Applications

- **paint.ml** - Interactive paint program with drawing tools
- **lightbulb.ml** - Simple state toggle demonstration
- **gdemo.ml** - Graphics primitives showcase
- **pairdemo.ml** - Widget pairing example

## Building & Running

Build the project:
```bash
make
```

Run the paint application:
```bash
make paint
```

Run tests:
```bash
make test
```

## Features

### Widget System
- Hierarchical widget composition
- Event propagation and handling
- Layout management
- State management

### Paint Application
- Multiple drawing tools (line, point, thick line)
- Color picker
- Undo/redo functionality
- Interactive canvas

## Course Information

Part of CIS 1200 at the University of Pennsylvania

- [Homework Description](http://www.cis.upenn.edu/~cis1200/current/hw/hw05/)
- [FAQ](https://www.seas.upenn.edu/~cis1200/current/hw/hw05/faq/)
- [Codio Documentation](http://www.cis.upenn.edu/~cis1200/current/codio)
