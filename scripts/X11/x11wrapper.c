#include <X11/Xlib.h>

Status sendInput(Display * display, Window win, Window root, int pressed, int keycode) {
  XKeyEvent event;

  event.type        = (pressed ? KeyPress : KeyRelease);
  event.display     = display;
  event.window      = win;
  event.root        = root;
  event.subwindow   = None;
  event.time        = CurrentTime;
  event.x           = 1;
  event.y           = 1;
  event.x_root      = 1;
  event.y_root      = 1;
  event.same_screen = True;
  event.keycode     = XKeysymToKeycode(display, keycode);
  event.state       = 0; // modifiers

  return XSendEvent(display, win, True, KeyPressMask, (XEvent*)&event);
}
