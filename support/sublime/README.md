# Projector Sublime Text 3 integration

This provides some rudimentary `.prj` syntax highlighting for Sublime Text 3.

## Usage

As long as the `Projector HTML.sublime-syntax` file is in `Packages`,
Sublime should automatically use it for any `.prj` file. It should
also appear in the `View -> Syntax` menu as `Projector HTML`.

`cp *.sublime-syntax ~/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/`

If you plan on hacking / improving on the editor integration, consider
symlinking to this git repo instead of a copy:

`ln -s "$(pwd)/Projector\ HTML.sublime-syntax" "$HOME/Library/Application\ Support/Sublime\ Text\ 3/Packages/User`
