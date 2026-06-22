/* genqoi.c -- regenerates test/fixtures/reference.qoi using the OFFICIAL QOI
 * reference encoder, so the committed fixture is genuinely third-party.
 *
 * Build:
 *   curl -fsSL https://raw.githubusercontent.com/phoboslab/qoi/master/qoi.h -o qoi.h
 *   cc -O2 -o genqoi genqoi.c && ./genqoi    # writes reference.qoi
 *
 * The pixel() pattern below is mirrored byte-for-byte by Support.referenceImage
 * in test/support.sml; the test suite asserts (a) Qoi.decode of this fixture
 * reproduces that image and (b) Qoi.encode of that image reproduces this file
 * exactly. qoi.h is MIT / public domain (c) Dominic Szablewski. */
#include <stdio.h>
#include <stdlib.h>
#define QOI_IMPLEMENTATION
#include "qoi.h"

/* Deterministic test image, exercising every QOI op:
   RUN (solid band), DIFF (x increments by 1), LUMA/RGB (larger steps),
   INDEX (cycling 4-color palette), RGBA (varying alpha). */
static void pixel(int x, int y, unsigned char *o) {
    unsigned char r,g,b,a;
    if (y < 6)        { r=10;  g=20;  b=30;  a=255; }
    else if (y < 12)  { r=(unsigned char)x; g=64; b=128; a=255; }
    else if (y < 18)  { r=(unsigned char)(x*3); g=(unsigned char)x; b=(unsigned char)(x*5); a=255; }
    else if (y < 24)  {
        int k=(x+y)&3;
        switch(k){
            case 0: r=200;g=10; b=10; a=255; break;
            case 1: r=10; g=200;b=10; a=255; break;
            case 2: r=10; g=10; b=200;a=255; break;
            default:r=200;g=200;b=10; a=255; break;
        }
    }
    else { r=(unsigned char)(x*8); g=(unsigned char)(y*8); b=(unsigned char)(x*y); a=(unsigned char)(x*8); }
    o[0]=r; o[1]=g; o[2]=b; o[3]=a;
}

int main(void) {
    int w=32, h=32;
    unsigned char *px = malloc((size_t)w*h*4);
    for (int y=0;y<h;y++)
        for (int x=0;x<w;x++)
            pixel(x,y,&px[(y*w+x)*4]);

    qoi_desc desc = { .width=(unsigned)w, .height=(unsigned)h, .channels=4, .colorspace=0 };
    int len=0;
    void *enc = qoi_encode(px, &desc, &len);
    if (!enc) { fprintf(stderr,"encode failed\n"); return 1; }

    FILE *f = fopen("reference.qoi","wb");
    fwrite(enc, 1, (size_t)len, f);
    fclose(f);
    fprintf(stderr,"wrote reference.qoi (%d bytes)\n", len);
    free(enc); free(px);
    return 0;
}
