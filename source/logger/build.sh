#!/bin/bash

OPTIMIZE=-O9

# general miscellaneous functions
if [[ "$(uname)" == "Darwin" ]] ; then
	DARWIN=yes
	UPPER_CHARS='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
	LOWER_CHARS='abcdefghijklmnopqrstuvwxyz'
	# fake the CI/CD environment variables for testing
	CI_PROJECT_TITLE="${PWD##*/}"
	CI_PROJECT_NAME="local-${PWD##*/}"
	CI_COMMIT_BRANCH='default'
	GITLAB_USER_NAME="${USER}"
else
	unset DARWIN
fi

function upperCase () {
	local s="${*}"
	if [[ ${DARWIN} ]] ; then
		# unfortunately cannot use the simple ${var^^} on OS-X to convert to upper case
	    local out c i
		for (( i=0;i<${#s};i++ )) ; do
			c="${s:${i}:1}"
			if [[ "${LOWER_CHARS//${c}}" != "${LOWER_CHARS}" ]] ; then
				c="${LOWER_CHARS%${c}*}"
				c="${UPPER_CHARS:${#c}:1}"
			fi
			out="${out}${c}"
		done
		echo "${out}"
	else
		echo "${s^^}"
	fi
}

function lowerCase () {
	local s="${*}"
	if [[ ${DARWIN} ]] ; then
		# unfortunately cannot use the simple ${var,,} on OS-X to convert to lower case
		local out c i t
		for (( i=0;i<${#s};i++ )) ; do
			c="${s:${i}:1}"
			if [[ "${c//[A-Z]}" != "${c}" ]] ; then
				t="${UPPER_CHARS%${c}*}"
				t="${LOWER_CHARS:${#t}:1}"
				[[ "${t}" != "" ]] && c="${t}"
			fi
			out="${out}${c}"
		done
		echo "${out}"
	else
		echo "${s,,}"
	fi
}

function die () {

	local ret=$?
	echo "ERROR: compile ${1} failed."
	exit $ret

}

function build () {

	local s="${1}"
	local e="$(upperCase ${2##*.})"
	local n="${s##*/}"
	n="${n%.*}"
	local o="$(upperCase ${2})"
	shift 2
	echo "Building ${n}......${e}"
	[[ -f "${o}" ]] && rm "${o}"
	nasm -s "${s}" -ILIBS/ -fbin ${OPTIMIZE} ${@} -o "${o}" || die "${n}"
	if [ ! -f  "${o}" ] ; then
		die "${n}"
	fi
	if [[ "${e}" != 'BIN' ]] ; then
		if [ ! -d ../../BIN ] ; then
			mkdir ../../BIN || die "${n}"
		fi
		if [ -f "../../BIN/${o}" ] ; then
			ls -al "../../BIN/${o}"
		fi
		cp "${o}" "../../BIN/${o}"
		ls -al "../../BIN/${o}"
	else
		ls -al "${o}"
	fi
}

function wct () {
	local t=$(wc ${@} *.asm *.ASM *.inc *.INC 2>/dev/null | grep -i ' total')
	echo ${t% *}
}

function build_main () {

	echo "WARNING: NASM 2.15.05, or later is recommended for compilation."

	if [[ "${1}" == 'single' ]] ; then
		rm ../../BIN/*.SYS >/dev/null 2>&1
		rm *.SYS >/dev/null 2>&1
		build "log-drvr.asm" 'logger.bin' -DSINGLE_BINARY
		build "log-intf.asm" 'logger.com' -DSINGLE_BINARY
	else
		rm *.BIN >/dev/null 2>&1
		build "log-drvr.asm" 'logger.sys'
		build "log-intf.asm" 'logger.com'
	fi

	echo "$(wct -l) lines of source code ($(( $(wct -c) / 1024 )) kbytes)"
}

build_main $@
