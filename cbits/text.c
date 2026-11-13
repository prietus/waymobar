/* Tabs as table columns: every tab stop is placed just after the widest
 * cell before it, over all the lines of the layout. */
#include <pango/pango.h>

#define MAX_COLS 16

void waymobar_align_tabs(PangoLayout *layout, int gap)
{
    const char *text = pango_layout_get_text(layout);
    int cols = 0, col = 0, k, i;
    for (i = 0; text[i]; i++) {
        if (text[i] == '\n')
            col = 0;
        else if (text[i] == '\t' && ++col > cols)
            cols = col;
    }
    if (cols == 0)
        return;
    if (cols > MAX_COLS)
        cols = MAX_COLS;
    PangoTabArray *tabs = pango_tab_array_new(cols, FALSE);
    /* Where the k-th tab of a line starts depends only on the stops before
     * it, so the stops are found from left to right. */
    for (k = 0; k < cols; k++) {
        int max = 0;
        col = 0;
        for (i = 0; text[i]; i++) {
            if (text[i] == '\n') {
                col = 0;
            } else if (text[i] == '\t') {
                if (col == k) {
                    PangoRectangle r;
                    pango_layout_index_to_pos(layout, i, &r);
                    if (r.x > max)
                        max = r.x;
                }
                col++;
            }
        }
        pango_tab_array_set_tab(tabs, k, PANGO_TAB_LEFT, max + gap * PANGO_SCALE);
        PangoTabArray *sofar = pango_tab_array_copy(tabs);
        pango_tab_array_resize(sofar, k + 1);
        pango_layout_set_tabs(layout, sofar);
        pango_tab_array_free(sofar);
    }
    pango_tab_array_free(tabs);
}
