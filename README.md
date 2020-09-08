# Evolution Gaming Scala Bootcamp Home Task

This is my implementation of the home task I was given.

## Setup

1. Install JDK 8 or 11 and Scala Build Tool (sbt).

## Usage

1. Open Bash Shell in the project directory, where the build.sbt file is.
2. Start sbt shell

```$ sbt
```

3. Compile a project
```$ sbt
sbt:scala> compile
```
4. Run the program
```$sbt
sbt:scala> run
```

5. Input data

```4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d
2h3h4h5d8d KdKs 9hJh
```

6. Input empty line to end the stdin loop

7. Output:

```Ac4d=Ad4s 5d6d As9s KhKd
KdKs 9hJh
```

## Options

### --omaha

This implementation supports Omaha Holdem Poker. To run the program in omaha mode, type:

```$sbt
sbt:scala> run --omaha
```

Made by Mihails Jeremejevs (kabbandre@gmail.com)
