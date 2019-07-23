## Play audio in Common Lisp
Load audio with FFMPEG or libsndfile, and play it with OpenAL. 
FFMPEG supports a wide range of sound formats, but libsndfile is smaller and on quicklisp. 

## Technical details

```/ffmpeg/```:

- ```ffmpeg.lisp``` uses the ffmpeg API to load packets from sound files. 

- ```ffmpeg-bindings.lisp``` is hand-written bindings to ffmpeg, because I could not find up-to-date bindings for ffmpeg in common lisp at the time.

```/openal-and-ffmpeg/``` :

- `al.lisp` is bindings to the OpenAL C API.

- `openal.lisp` sets up OpenAL for playing audio, as well as background threads for buffering audio data and polling OpenAL.

```/musical-binaries/``` :

- contains FFMPEG and OpenAL binaries for Mac, Windows 32-bit, Windows 64-bit, and linux.

- Audio player is openal but a different backend could probably be used. 

## Bugs

FFMPEG:

The AVFormatContext c structure might need some tweaking depending on the version, since it uses (in the C source code) c macros to detect the operating system, so the structure might be different on different operating systems. There is a #+darwin in one of the files in this project for mac osx, it worked last time, but is now commented out and somehow works on linux. I am too lazy to figure out what happened.
