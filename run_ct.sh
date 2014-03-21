#!/bin/bash -ex
ct_run -dir apps/directory/test/ -include ../include \
    -suite directory_SUITE -logdir apps/directory/test/logs #-ct_hooks cth_log_redirect
