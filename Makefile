CC = ./rebar

all: deps lager_rsyslog

deps:
	$(CC) get-deps

lager_rsyslog:
	$(CC) compile

clean:
	$(CC) clean
