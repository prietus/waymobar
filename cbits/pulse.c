/* Volume of the default sink through libpulse (PulseAudio, or PipeWire
 * through pipewire-pulse). A threaded main loop follows server and sink
 * events; every change is announced on an eventfd that the Haskell side
 * waits on, and the state is read with waymobar_pulse_get. */
#include <pulse/pulseaudio.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/time.h>
#include <string.h>
#include <sys/eventfd.h>
#include <unistd.h>

static pa_threaded_mainloop *ml;
static pa_context *ctx;
static int efd = -1;

/* protected by the main loop lock */
static int have_sink;
static int cur_volume;
static int cur_muted;
static pa_cvolume cur_cvolume;
static char cur_sink[256];
static char cur_desc[256];

static void connect_ctx(void);

static void notify(void)
{
    uint64_t one = 1;
    if (write(efd, &one, sizeof one) < 0) {
        /* nothing to do: the reader is behind, it will read the state anyway */
    }
}

static void sink_cb(pa_context *c, const pa_sink_info *i, int eol, void *u)
{
    (void)c; (void)u;
    if (eol || !i)
        return;
    have_sink = 1;
    cur_cvolume = i->volume;
    cur_volume = (int)((pa_cvolume_avg(&i->volume) * 100.0 + PA_VOLUME_NORM / 2) / PA_VOLUME_NORM);
    cur_muted = i->mute;
    snprintf(cur_sink, sizeof cur_sink, "%s", i->name ? i->name : "");
    snprintf(cur_desc, sizeof cur_desc, "%s", i->description ? i->description : "");
    notify();
}

static void server_cb(pa_context *c, const pa_server_info *i, void *u)
{
    (void)u;
    if (!i || !i->default_sink_name) {
        have_sink = 0;
        notify();
        return;
    }
    pa_operation *o = pa_context_get_sink_info_by_name(c, i->default_sink_name, sink_cb, NULL);
    if (o)
        pa_operation_unref(o);
}

static void refresh(void)
{
    pa_operation *o = pa_context_get_server_info(ctx, server_cb, NULL);
    if (o)
        pa_operation_unref(o);
}

static void subscribe_cb(pa_context *c, pa_subscription_event_type_t t, uint32_t idx, void *u)
{
    (void)c; (void)t; (void)idx; (void)u;
    refresh();
}

static void reconnect_cb(pa_mainloop_api *api, pa_time_event *e, const struct timeval *tv, void *u)
{
    (void)tv;
    api->time_free(e);
    /* the failed context is released here, outside its own callbacks */
    if (u)
        pa_context_unref((pa_context *)u);
    connect_ctx();
}

static void state_cb(pa_context *c, void *u)
{
    (void)u;
    switch (pa_context_get_state(c)) {
    case PA_CONTEXT_READY:
        pa_context_set_subscribe_callback(c, subscribe_cb, NULL);
        pa_operation_unref(pa_context_subscribe(c, PA_SUBSCRIPTION_MASK_SINK | PA_SUBSCRIPTION_MASK_SERVER, NULL, NULL));
        refresh();
        break;
    case PA_CONTEXT_FAILED:
    case PA_CONTEXT_TERMINATED: {
        /* the server went away (or was never there): retry in 2 seconds */
        if (c != ctx)
            break;
        have_sink = 0;
        notify();
        pa_context_set_state_callback(c, NULL, NULL);
        pa_context_set_subscribe_callback(c, NULL, NULL);
        ctx = NULL;
        pa_mainloop_api *api = pa_threaded_mainloop_get_api(ml);
        struct timeval tv;
        gettimeofday(&tv, NULL);
        tv.tv_sec += 2;
        api->time_new(api, &tv, reconnect_cb, c);
        break;
    }
    default:
        break;
    }
}

static void connect_ctx(void)
{
    ctx = pa_context_new(pa_threaded_mainloop_get_api(ml), "waymobar");
    pa_context_set_state_callback(ctx, state_cb, NULL);
    /* a failure, even an immediate one, goes through state_cb */
    pa_context_connect(ctx, NULL, PA_CONTEXT_NOAUTOSPAWN, NULL);
}

/* Start following the default sink; returns the eventfd, or -1. */
int waymobar_pulse_start(void)
{
    if (efd >= 0)
        return efd;
    efd = eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK);
    if (efd < 0)
        return -1;
    ml = pa_threaded_mainloop_new();
    if (!ml)
        return -1;
    pa_threaded_mainloop_lock(ml);
    connect_ctx();
    pa_threaded_mainloop_unlock(ml);
    if (pa_threaded_mainloop_start(ml) < 0)
        return -1;
    return efd;
}

/* 1 and the state if there is a default sink, 0 otherwise. */
int waymobar_pulse_get(int *volume, int *muted, char *desc, int desclen)
{
    if (!ml)
        return 0;
    pa_threaded_mainloop_lock(ml);
    int ok = have_sink;
    *volume = cur_volume;
    *muted = cur_muted;
    snprintf(desc, desclen, "%s", cur_desc);
    pa_threaded_mainloop_unlock(ml);
    return ok;
}

int waymobar_pulse_toggle_mute(void)
{
    if (!ml)
        return 0;
    pa_threaded_mainloop_lock(ml);
    int ok = have_sink && ctx;
    if (ok) {
        pa_operation *o = pa_context_set_sink_mute_by_name(ctx, cur_sink, !cur_muted, NULL, NULL);
        if (o)
            pa_operation_unref(o);
    }
    pa_threaded_mainloop_unlock(ml);
    return ok;
}

/* Change the volume by delta percent, never above max percent. */
int waymobar_pulse_change(int delta, int max)
{
    if (!ml)
        return 0;
    pa_threaded_mainloop_lock(ml);
    int ok = have_sink && ctx;
    if (ok) {
        pa_cvolume v = cur_cvolume;
        pa_volume_t step = (pa_volume_t)((double)PA_VOLUME_NORM * (delta < 0 ? -delta : delta) / 100.0);
        if (delta > 0)
            pa_cvolume_inc_clamp(&v, step, (pa_volume_t)((double)PA_VOLUME_NORM * max / 100.0));
        else
            pa_cvolume_dec(&v, step);
        pa_operation *o = pa_context_set_sink_volume_by_name(ctx, cur_sink, &v, NULL, NULL);
        if (o)
            pa_operation_unref(o);
    }
    pa_threaded_mainloop_unlock(ml);
    return ok;
}
