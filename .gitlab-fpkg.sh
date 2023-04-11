#!/bin/bash

# Automatic FreeDOS Packaging Utility for GitLab CI/CD
# Copyright (c) 2022 Jerome Shidel
# MIT License

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# This Utilities Version
PACK_VER='0.4.4'

# The Darwin related functionality in this script is for the development and
# local testing of the script itself on my Mac. That functionality is not
# intended for use in an actual CI/CD environment on GitLab/GitHub. If somehow
# the CI/CD is running on a Mac (which I doubt), the code in this script
# that is related to Darwin will need modified for the script to function
# properly.

# See the LSMtoVars function for nearly all CI and LSM related variables that
# are created for the package build. Including the $PKG_FREEDOS url for
# downloading the final zip archive for the related the release.

# This script also creates a modified LSM file with these fields:
# Packager:      {this script} {script version}
# Modified-date: {commit time}.0
# Git-Site:      {url for gitlab archive project}
# Git-Commit:    {short has of commit that generated the release}
# LFN-Files:	 {if this field already exists, it is assumed to be correct}
#		 {when not present, project is scanned. If no LFN's are found
# 		 outside the source directory, the field is not added and the
#		 project is compressed using DOS compatibility}
#		 {if LFN's are found in a primary system directory, it is set to
#		 "required" and project is compressed with LFN support}
#		 {if LFN's are found in a non-system, non-primary directory,
#		 this field is set to "compressed" and those files are moved
#		 into a LFNFILES.ZIP archive in that directory and the project
#		 is compressed in DOS mode. Files under SOURCES are not scanned
#		 and are always pre-compressed into an archive supporting LFNs. }
# Timestamps:	 {if no issues, field is not included}
#		 {no timestamp file, set to "unsupported" }
#		 {if there is a file hash mismatch or a file is not present in
#		 the .timestamps database file, set to "outdated". This will
#		 occur when the project is pushed without using fdvcs.sh or
#		 compatible utility to maintain the timestamp preservation
#		 database.}
#
# TO-DO:
#
#	* Figure out how to run validate-job, but not release job when LSM
#	version has not been updated.
#
#	* Ensure that package artifact has a permanent link to the asset.
#
#	* figure out how to have package download directly from release link
#	without going to a separate download page.

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

function caseless () {
	local t="$(upperCase ${@})"
	local out x i u f
	while [[ ${#t} -ne 0 ]] ; do
		x="${t%%/*}"
		t="${t:${#x}}"
		t="${t:1}"
		f=''
		for i in "${out}"* ; do
			u="$(upperCase ${i:${#out}})"
			if [[ "${u}" == "${x}" ]] ; then
		   		f="${i:${#out}}"
		   		break
		   	fi
		done
		if [[ "${f}" == '' ]] ; then
		   	# echo "unable to match path/filename \"${@}\"" >&2
		   	return 1
		fi
		out="${out}${f}"
		[[ ${#t} -ne 0 ]] && out="${out}/"
	done
	echo "${out}"
	return 0
}

function crlf () {
	(
	        export LC_CTYPE=C
        	tr -d '\r' | perl -pe 's/\n/\r\n/'
        )
}

function NoCtrlChars () {
    (
        export LC_CTYPE=C
        tr -d '[:cntrl:]'
    )
}


function divider () {
	local bar='----'
	while [[ ${#bar} -lt 40 ]] ; do
		bar="${bar}${bar}"
	done
	echo "${bar}"
	echo "${@}"
}

function fileHash () {
	local sha=$(shasum -a 512 "${1}" 2>/dev/null)
	sha="${sha%% *}"
	if [[ "${sha}" == '' ]] ; then
		echo "unable to hash file '${1}'" >&2
		return 1
	fi
	echo "${sha}"
	return 0
}

function epochToDate () {
    if [[ ${DARWIN} ]] ; then
        # macOS Version
        date -r ${1} +"%C%y%m%d%H%M.%S"
    else
        # Linux Version
        date -d @${1} +"%C%y%m%d%H%M.%S"
    fi
}

# Directory in CI/CD container where the FreeDOS package will be assembled
PKG_ROOT="package/${CI_PROJECT_TITLE}"

# Multi-App package, relaxes some restrictions to what files can be included
# in what directories.
MULTI_APP=false

# Whether or not SOURCE files would always be pre-compressed. Disabling this
# can cause side effects when trying to build the program from source.
ZIP_SOURCES=true

# include additional script configuration if present
if [[ -f '.gitlab-fpkg.cfg' ]] ; then
	divider 'include custom configuration settings from .gitlab-fpkg.cfg'
	. ./.gitlab-fpkg.cfg || exit $?
	echo "MULTI_APP=${MULTI_APP}"
	echo "ZIP_SOURCES=${ZIP_SOURCES}"
fi

# script specific functions
function requireExist () {
	local t="$(caseless ${2})"
	if [[ "${1}" == '-d' ]] && [[ ! -d "${t}" ]]; then
		echo "project missing \"${2}\" directory" >&2
		return 1
	fi
	if [[ "${1}" == '-f' ]] && [[ ! -f "${t}" ]]; then
		echo "project missing \"${2}\" file" >&2
		return 1
	fi
	return 0
}

function hasNoFiles () {
	local f
	for f in "${1}"/* ; do
		if [[ -f "${f}" ]] ; then
			echo "files exist in primary directory \"${1}\""
			return 1
		fi
	done
	return 0
}

function singleLSM () {
	local c=0
	local e i a="$(caseless appinfo)"
	for i in "${a}"/* ; do
		e="$(upperCase ${i##*.})"
		if [[ "${e}" = 'LSM' ]] ; then
			(( c++ ))
			if [[ ${c} -gt 1 ]] ; then
				echo "project contains multiple LSM metadata files" >&2
				return 1
			fi
		fi
	done
	return 0
}

function checkSubDirs () {
	local f t
	for f in * ; do
		[[ ! -d "${f}" ]] && continue
		t="$(upperCase ${f})"
		[[ "${t}" == 'APPINFO' ]] && continue
		[[ "${t}" == 'BIN' ]] && continue
		[[ "${t}" == 'NLS' ]] && continue
		[[ "${t}" == 'HELP' ]] && continue
		[[ "${t}" == 'LINKS' ]] && continue
		[[ "${t}" == 'BATLINKS' ]] && continue
		hasNoFiles "${f}" || return $?
		requireExist -d "${f}/${CI_PROJECT_TITLE}" || return 1
	done
	return 0
}

function checkSysDir () {
	local p="$(caseless ${1})"
	if [[ ! -d "${p}" ]] ; then
		echo "project does not contain system directory \"${1}\", fine"
		return 0
	fi
	local n="$(upperCase ${CI_PROJECT_TITLE})"
	local f t
	for f in "${p}"/* ; do
		if [[ -d "${f}" ]] ; then
			echo "project contains a subdirectory \"${f##*/}\" under system directory \"${1}\""
			return 1
		fi
		t="$(upperCase ${f##*/})"
		t="${t%.*}"
		if [[ "${n}" != "${t}" ]] ; then
			echo "invalid file \"${f##*/}\" detected under system directory \"${1}\""
			return 1
		fi
	done
	return 0
}

function checkBatDir () {
	local p="$(caseless ${1})"
	if [[ ! -d "${p}" ]] ; then
		echo "project does not contain system directory \"${1}\", fine"
		return 0
	fi
	local f t
	for f in "${p}"/* ; do
		if [[ -d "${f}" ]] ; then
			echo "project contains a subdirectory \"${f##*/}\" under system directory \"${1}\""
			return 1
		fi
		t="$(upperCase ${f##*/})"
		t="${t#*.}"
		if [[ "${t}" != 'BAT' ]] ; then
			echo "invalid file \"${f##*/}\" detected under system directory \"${1}\""
			return 1
		fi
	done
	return 0
}

function commitHash () {
	if [[ "${CI_COMMIT_SHORT_SHA}" != '' ]] ; then
		echo "${CI_COMMIT_SHORT_SHA}"
	else
		git rev-parse --verify --short=8 HEAD
	fi
}

function testLFN () {
	local x t
	x="${1}"
	t="${x%.*}"
	[[ ${#t} -gt 8 ]] && return 0
	t="${x##*.}"
	[[ "${t}" != "${x}" ]] && [[ ${#t} -gt 3 ]] && return 0
	x="${x//[[:alnum:]]}"
	x="${x/.}"
	x="${x//[~!@#$%^&+=\-_]}"
	[[ ${x} ]] && return 0
	return 1
}

function scannerLFN () {
	local i req x p="$(upperCase ${1:${#SCAN_BASE}})"
	p="${p:1:$(( ${#p} - 2))}"
	[[ "${p}" == "SOURCE" ]] && return 0
	[[ "${p}" == "${p//\/}" ]] && p= || p=true
	if [[ "${1}" != '' ]] ; then
		pushd "${1}" 2>&1 >/dev/null || return 1
	fi

	for i in "${1}"* ; do
		req=${2}
		testLFN "${i:${#1}}" && req=true
		if [[ ${req} ]] ; then
			if [[ ! ${p} ]] ; then
				LSM_LFN=required
				echo "[required] ${i:${#SCAN_BASE}}"
			else
				echo "[compress] ${i:${#SCAN_BASE}}"
				if [[ ! ${LSM_LFN} ]] ; then
					LSM_LFN=compressed
				fi
			fi
		fi
		[[ -d "${i}" ]] && scannerLFN "${i}/" ${req}
	done

	if [[ "${1}" != '' ]] ; then
		popd 2>&1 >/dev/null || return 1
	fi
	return 0
}

function scanLFN () {
	divider "Performing long file name requirement scan"
	SCAN_BASE="${PWD}"
	local i
	for i in "${SCAN_BASE}/"* ; do
		if [[ -d "${i}" ]] ; then
			scannerLFN "${i}/" || return $?
		fi
	done
	if [[ "${LSM_LFN}" == "compressed" ]] ; then
		echo 'package will compress optional long file names.'
	elif [[ "${LSM_LFN}" ]] ; then
		echo 'package will require long file name support.'
	else
		echo 'no files or directories requiring LFN support detected.'
	fi
 	return 0
}

function zipAdd () {
	if [[ -d "${1}" ]] ; then
		echo "DIR:  ${BASE_DIR}/${1} > ${ZIP_SHOW}"
		zip -m -r -9 "${ZIP_FILE}" "${1}" || return 1
	else
		echo "FILE: ${BASE_DIR}/${1} > ${ZIP_SHOW}"
		zip -m -9 "${ZIP_FILE}" "${1}" || return 1
	fi
	return 0

}

function zipLFN () {
	local i
	for i in "${1}"* ; do
		testLFN "${i:${#1}}"
		if [[ $? -eq 0 ]] ; then
			zipAdd "${i}" || return 1
		elif [[ -d "${i}" ]] ; then
			zipLFN "${i}/"
		fi
	done
}

function compressorMain () {
	BASE_DIR="${1}"
	pushd "${1}" 2>&1 >/dev/null || return 1
	ZIP_FILE="${PWD}/LFNFILES.ZIP"
	ZIP_SHOW="${1}/LFNFILES.ZIP"
	zipLFN ""
	popd 2>&1 >/dev/null || return 1
}

function compressorLFN() {
	local i
	for i in "${1}"* ; do
		if [[ -d "${i}" ]] ; then
			compressorMain "${i}" || return $?
		fi
	done
 	return 0
}

function compressLFN () {
	divider "pre-compress files requiring LFN support"
	local i
	for i in * ; do
		if [[ -d "${i}" ]] ; then
			compressorLFN "${i}/" || return $?
		fi
	done
 	return 0
}

function LSMtoVars () {
	if [[ $CI_COMMIT_BRANCH == $CI_DEFAULT_BRANCH ]] ; then
		DEPLOY_RELEASE=true
	else
		unset DEPLOY_RELEASE
	fi

	local l v d o="$(caseless appinfo/${CI_PROJECT_TITLE}.lsm)"
	while read -r l ; do
		l="${l//[[:cntrl:]]}"
		v="$(upperCase ${l%%:*})"
		v="${v//-/_}"
		[[ "${v}" != 'VERSION' ]] && [[ "${v}" != 'LFN_SUPPORT' ]] && continue
		d="${l:${#v}}"
		d="${d:1}"
		d="${d#${d%%[![:space:]]*}}"
		d="${d//\|}"
		d="${d//\'}"
		if [[ "${v}" == 'VERSION' ]] ; then
			TAG="v${d// /-}"
			TAG="${TAG//[()\\\/]}"
			TAG="${TAG//}"
			echo "TAG=${TAG}"  >> variables.env
		elif [[ "${v}" == 'LFN_SUPPORT' ]] ; then
			echo "LSM_LFN=${d}"  >> variables.env
			LSM_LFN=v${d}
		fi
		d="LSM_${v}='${d}'"
		eval "${d}" || continue
		echo "${d}" >> variables.env
	done< "${o}"
	LSM_COMMIT="#$(commitHash)"
	if [[ ! ${LSM_LFN} ]] ; then
		scanLFN || return $?
		[[ ${LSM_LFN} ]] && echo "LSM_LFN=${LSM_LFN}"  >> variables.env
	fi
	echo "LSM_COMMIT=${LSM_COMMIT}"  >> variables.env
	echo "LSM_BUILD_ID=${CI_JOB_ID}"  >> variables.env
	git rev-parse --verify refs/tags/${TAG} >/dev/null 2>&1 && DEPLOY_RELEASE=false
	if [[ ${DEPLOY_RELEASE} == true ]] ; then
		echo "DEPLOY_RELEASE=true"  >> variables.env
		# echo "PKG_DOWNLOAD=${CI_PROJECT_URL}/-/jobs/${CI_JOB_ID}/artifacts/file/package/${CI_PROJECT_TITLE}" >> variables.env
		# echo "PKG_OTHER=${CI_PROJECT_URL}/-/releases/${TAG}/downloads/${PKG_ROOT}" >> variables.env
		echo "PKG_FREEDOS=${CI_PROJECT_URL}/-/jobs/${CI_JOB_ID}/artifacts/file/${PKG_ROOT}.zip" >> variables.env
		# [[ ${DEPLOY_RELEASE} == false ]] && echo 'CI_DEPLOY_FREEZE=true' >> variables.env
	else
		unset DEPLOY_RELEASE
	fi
	divider 'Custom Environment variables:'
	cat variables.env
	return 0
}

function applyTimestamps () {
	LSM_STAMPS=''
	if [[ ! -f .timestamps ]] ; then
		divider 'no timestamp file'
		LSM_STAMPS='unsupported'
		return 0
	fi
	divider 'Apply recorded timestamps:'
	git ls-files > .timestamps-files
	local line stamp hash fhash
	while IFS=""; read -r line ; do
		[[ "${line:0:1}" == '#' ]] && continue
		[[ "${line}" == '' ]] && continue
		stamp="${line%% *}"
		line="${line:$(( ${#stamp} + 1 ))}"
		hash="${line%% *}"
		line="${line:$(( ${#hash} + 1 ))}"
		if [[ ! -f "${line}" ]] ; then
			echo "missing file: ${line}"
			continue
		fi
		echo "${line}" >> .timestamps-files
		fhash=$(fileHash "${line}")
		if [[ "${hash}" != "${fhash}" ]] ; then
			echo "hash mismatch:  ${line}"
			LSM_STAMPS='outdated'
			continue
		fi
		stamp=$(epochToDate ${stamp})
		echo ${stamp} "${line}"
		touch -t ${stamp} "${line}" || echo " error"
	done < .timestamps
	cat .timestamps-files | sort > .timestamps-temp
	echo >> .timestamps-temp
	local x
	while IFS=""; read -r line ; do
		[[ "${line}" == "" ]] && continue
		[[ "${line}" == ".timestamps" ]] && continue
		if [[ "${x}" == "${line}" ]] ; then
			x=
			continue
		fi
		if [[ "${x}" != '' ]] ; then
			echo "no timestamp: ${x}"
			LSM_STAMPS='outdated'
		fi
		x="${line}"
	done < .timestamps-temp
	rm .timestamps-temp .timestamps-files
	[[ ${LSM_STAMPS} ]] && echo "timestamp data is ${LSM_STAMPS}"
	return 0
}

# main script
function packager_version () {
	divider "Automatic FreeDOS Packaging Utility v${PACK_VER} for GitLab CI/CD"
	echo "CI/CD for \"${CI_PROJECT_TITLE}\" (${CI_PROJECT_NAME}) on ${CI_COMMIT_BRANCH} branch."
	echo "triggered by ${GITLAB_USER_NAME} with commit #$(commitHash)."
}

function test_structure () {
	divider 'Validate project structure:'
	requireExist -d "appinfo" || return $?
	requireExist -f "appinfo/${CI_PROJECT_TITLE}.lsm" || return $?
	if [[ "${ZIP_SOURCES}" != 'exclude' ]] ; then
		requireExist -d "source/${CI_PROJECT_TITLE}" || return $?
	fi
	singleLSM || return $?
	if [[ ${RELAXED_PATHS} != true ]] ; then
		checkSubDirs || return $?
	fi
	checkSysDir APPINFO || return $?
	if [[ ${MULTI_APP} == true ]] ; then
		echo 'MULTI-APP, relaxed file name validation in system directories'
	else
		checkSysDir HELP || return $?
		checkSysDir NLS || return $?
	fi;
	checkBatDir LINKS || return $?
	checkBatDir BATLINKS || return $?

	echo 'project structure validated'
	return 0
}

function printLSM () {
	echo 'Begin3'
	grep -iv "^begin3\|^end\|^modified-date\|^git-site\|^git-commit\|^packager\|^lfn_support" "${1}"
	echo "Packager:       ${0##*/} v${PACK_VER}"
	echo "Modified-date:  ${CI_COMMIT_TIMESTAMP%%T*}.0"
	echo "Git-Site:       ${CI_PROJECT_URL}"
	echo "Git-Commit:     ${LSM_COMMIT//#}"
	[[ ${LSM_LFN} ]] && echo "LFN-Support:    ${LSM_LFN}"
	[[ ${LSM_STAMPS} ]] && echo "Timestamps:     ${LSM_STAMPS}"
	echo 'End'
}

function freshenLSM () {
	local lsm="$(caseless appinfo/${CI_PROJECT_TITLE}.lsm)"
	printLSM "${lsm}" | crlf > "${PKG_ROOT}/${lsm}"
	divider 'modified LSM metadata file:'
	cat "${PKG_ROOT}/${lsm}"
}

function compile_from_source () {
	[[ ! -x .gitlab-compile.sh ]] && return 0
	divider "compile executables for ${CI_PROJECT_TITLE}:"
	. ./.gitlab-compile.sh
	return $?
}

function build_package () {
	local i
	divider "build package $TAG:"
	mkdir -v "${PKG_ROOT%%/*}" || return $?
	mkdir -v "${PKG_ROOT}" || return $?
	for i in * ; do
	   [[ -f "${i}" ]] && continue
	   [[ "${i}" == "${PKG_ROOT%%/*}" ]] && continue
	   cp -av "${i}" "${PKG_ROOT}" || return $?
	done
	applyTimestamps || return $?
	freshenLSM || return $?
	return 0
}

function compress_package () {
	local srcs="$(caseless ${PKG_ROOT}/SOURCE/${CI_PROJECT_TITLE})"
	if [[ "${ZIP_SOURCES}" == 'exclude' ]] ; then
		divider "exclude sources is active, binaries only package"

	elif [[ ${ZIP_SOURCES} == false ]] ; then
		divider "pre-compress sources is disabled"
	else
		divider "pre-compress sources:"
		pushd "${srcs}" || return $?
		zip -r -v -9 "../SOURCES.ZIP" * || return $?
		popd
		rm -rf "${srcs}" || return $?
		mkdir "${srcs}" || return $?
		mv -v "$(caseless ${PKG_ROOT}/SOURCE/SOURCES.ZIP)" "${srcs}" || return $?
	fi

	if [[ "${LSM_LFN}" ==  "compressed" ]] ; then
		pushd "$(caseless ${PKG_ROOT})" || return $?
		compressLFN || return 1
		popd
	fi

	if [[ ${LFN_SUPPORT} ]] ; then
		divider "create zip archive (case specific):"
		pushd "${PKG_ROOT}" || return $?
		zip -r -v -9 "../${PKG_ROOT##*/}.zip" * || return $?
	else
		divider "create zip archive (DOS compatible):"
		pushd "${PKG_ROOT}" || return $?
		zip -r -v -9 -k "../${PKG_ROOT##*/}.zip" * || return $?
	fi
	popd
	return 0
}


while [[ "${1}" != '' ]] ; do
	case "${1}" in
		'version')
			packager_version
		;;
		'validate')
			test_structure || exit $?
		;;
		'prepare')
			[[ -f variables.env ]] && rm variables.env
			LSMtoVars || exit $?
			if [[ ! ${DEPLOY_RELEASE} ]] ; then
				divider
				echo "release ${TAG} already exists"
				echo "## DO NOT DEPLOY RELEASE! ##"
				divider
				exit 0
			else
				divider "new release desired"
			fi
		;;
		'compile')
			compile_from_source || exit $?
		;;
		'build')
			build_package || exit $?
			compress_package || exit $?
		;;
		'confirm')
			divider 'confirm build'
		;;
		'release')
			divider "deploy release $TAG"
		;;
		'final')
			divider "released $TAG"
			divider
		;;
		'lfn')
			scanLFN || exit $?
			compressLFN
			exit 0
		;;
		'stamps')
			applyTimestamps || exit $?
			exit 0
		;;
		'all')
			${0} version validate prepare build confirm release final
			exit $?
		;;
		*)
		echo "invalid option: \"${1}\""
		exit 1
	esac
	shift
done

exit 0