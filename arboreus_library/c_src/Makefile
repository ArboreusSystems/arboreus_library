A_DIR_ROOT = $(shell pwd)
A_PRIV_DIR = ${A_DIR_ROOT}/../priv

A_MAKEFILE_CONFIG = ${A_DIR_ROOT}/../Makefile.conf
ifneq ("$(wildcard ${A_MAKEFILE_CONFIG})","")
  $(info using ${A_MAKEFILE_CONFIG})
  include ${A_MAKEFILE_CONFIG}
else
  $(error No config file ${A_MAKEFILE_CONFIG})
endif

INCLUDES += \
	-I${A_DIR_INCLUDES_ERLANG} \
	-I${A_DIR_ROOT}/a_time/headers \
	-I${A_DIR_ROOT}/a_universal/headers

DIR_LIBS +=

LIBS +=

A_UNIVERSAL_SOURCES += \
	${A_DIR_ROOT}/a_universal/a_convert.c \
	${A_DIR_ROOT}/a_universal/a_string.c \
	${A_DIR_ROOT}/a_universal/a_symbol.c

A_TIME_UNIVERSAL_SOURCES += \
	${A_UNIVERSAL_SOURCES} \
	${A_DIR_ROOT}/a_time/a_time_ansi.c \
	${A_DIR_ROOT}/a_time/a_time_month.c \
	${A_DIR_ROOT}/a_time/a_time_now_handler.c \
	${A_DIR_ROOT}/a_time/a_time_rfc822.c \
	${A_DIR_ROOT}/a_time/a_time_rfc850.c \
	${A_DIR_ROOT}/a_time/a_time_weekday.c

A_TIME_NOW_SOURCES += \
	${A_TIME_UNIVERSAL_SOURCES} \
	${A_DIR_ROOT}/a_time/a_time_now.c

A_TIME_SOURCES += \
	${A_TIME_UNIVERSAL_SOURCES} \
	${A_DIR_ROOT}/a_time/a_time.c


# ----------------------------------------------
# Targets

all: clean build_a_time_now_nif build_a_time

clean:

	rm -f ${A_PRIV_DIR}/a_time_now.so
	rm -f ${A_PRIV_DIR}/a_time

build_a_time_now_nif:
	echo "Building a_time_now NIF"
	gcc \
		-o ${A_PRIV_DIR}/a_time_now.so \
		-undefined dynamic_lookup -fPIC -shared \
		-O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes \
		-flat_namespace -undefined suppress \
		${A_TIME_NOW_SOURCES} \
		${INCLUDES} \
		${DIR_LIBS} \
		${LIBS}

build_a_time:
	echo "Building a_time application"
	gcc \
    	-o ${A_PRIV_DIR}/a_time \
    	-fPIC -O3 -std=c99 -finline-functions \
    	-Wall -Wmissing-prototypes \
    	${A_TIME_SOURCES} \
    	${INCLUDES} \
    	${DIR_LIBS} \
    	${LIBS}