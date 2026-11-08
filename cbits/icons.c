/* Load an image file (PNG, SVG, XPM, ... whatever gdk-pixbuf has loaders
 * for) as premultiplied native-endian ARGB32, the format cairo uses. */
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <stdint.h>
#include <stdlib.h>

unsigned char *waymobar_load_icon(const char *path, int size, int *out_w, int *out_h)
{
    GError *err = NULL;
    GdkPixbuf *pb = gdk_pixbuf_new_from_file_at_size(path, size, size, &err);
    if (!pb) {
        if (err)
            g_error_free(err);
        return NULL;
    }
    int w = gdk_pixbuf_get_width(pb), h = gdk_pixbuf_get_height(pb);
    int n = gdk_pixbuf_get_n_channels(pb), stride = gdk_pixbuf_get_rowstride(pb);
    gboolean alpha = gdk_pixbuf_get_has_alpha(pb);
    const guchar *src = gdk_pixbuf_read_pixels(pb);
    uint32_t *dst = malloc((size_t)w * h * 4);
    if (!dst) {
        g_object_unref(pb);
        return NULL;
    }
    for (int y = 0; y < h; y++) {
        const guchar *row = src + (size_t)y * stride;
        for (int x = 0; x < w; x++) {
            const guchar *p = row + x * n;
            uint32_t a = alpha ? p[3] : 255;
            uint32_t r = p[0] * a / 255, g = p[1] * a / 255, b = p[2] * a / 255;
            dst[(size_t)y * w + x] = (a << 24) | (r << 16) | (g << 8) | b;
        }
    }
    g_object_unref(pb);
    *out_w = w;
    *out_h = h;
    return (unsigned char *)dst;
}
