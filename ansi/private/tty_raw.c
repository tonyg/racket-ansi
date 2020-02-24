#include <stdio.h>
#include <stdbool.h>
#include <signal.h>
#include <termios.h>
#include <stdlib.h>
#include <unistd.h>

#define STDIN_FD 0

static bool is_raw = false;
static struct termios saved;

bool tty_raw(void) {
  /* Based on the settings given in:
   *  https://web.archive.org/web/20180516224400/http://www.minek.com:80/files/unix_examples/raw.html */
  struct termios t;

  if (is_raw) return true;

  if (tcgetattr(STDIN_FD, &saved) < 0) return false;
  t = saved;

  t.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  t.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  t.c_cflag &= ~(CSIZE | PARENB);
  t.c_cflag |= CS8;
  t.c_oflag &= ~(OPOST);
  t.c_cc[VMIN] = 1;
  t.c_cc[VTIME] = 0;

  if (tcsetattr(STDIN_FD, TCSAFLUSH, &t) < 0) return false;

  is_raw = true;
  return true;
}

bool tty_restore(void) {
  if (!is_raw) return true;

  if (tcsetattr(STDIN_FD, TCSAFLUSH, &saved) < 0) return false;

  is_raw = false;
  return true;
}

void tty_set_restore_at_exit(void) {
    atexit((void (*)(void)) tty_restore);
}
