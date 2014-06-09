#!/bin/bash -ex
ct_run -dir apps/session/test/ -include ../include \
    -suite session_SUITE -logdir apps/session/test/logs #-ct_hooks cth_log_redirect
