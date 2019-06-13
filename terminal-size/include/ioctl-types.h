#define TIOCGWINSZ 0x5413
#define TIOCGWINSZ 0x5401

struct winsize
  {
    unsigned short int ws_row;
    unsigned short int ws_col;
    unsigned short int ws_xpixel;
    unsigned short int ws_ypixel;
  };
