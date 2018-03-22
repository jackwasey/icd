
set -x

clean=""
user_dir=""
use_icd_home=""

optspec=":hv-:"
while getopts "$optspec" optchar; do
    case "${optchar}" in
        -)
            case "${OPTARG}" in
                clean)
                    echo "Will clean after installation" >&2;
                    clean=yes
                    ;;
                use-home)
                    echo "Will use ICD_HOME" >&2;
                    use_icd_home=yes
                    ;;
                dir)
                    val="${!OPTIND}"; OPTIND=$(( $OPTIND + 1 ))
                    echo "Parsing option: '--${OPTARG}', value: '${val}'" >&2;
                    user_dir="$val"
                    ;;
                dir=*)
                    val=${OPTARG#*=}
                    opt=${OPTARG%=$val}
                    echo "Parsing option: '--${opt}', value: '${val}'" >&2
                    user_dir="$val"
                    ;;
                *)
                    if [ "$OPTERR" = 1 ] && [ "${optspec:0:1}" != ":" ]; then
                        echo "Unknown option --${OPTARG}" >&2
                    fi
                    ;;
            esac;;
        h)
            echo "usage: $0 [-c] [--clean] " >&2
            exit 2
            ;;
        c)
            echo "Will clean after installation" >&2;
            clean=yes
            ;;
        *)
            if [ "$OPTERR" != 1 ] || [ "${optspec:0:1}" = ":" ]; then
                echo "Non-option argument: '-${OPTARG}'" >&2
            fi
            ;;
    esac
done

icd_install_tmp=$(mktemp -d)
dir="$icd_install_tmp"

if [ "$use_icd_home" = "yes" ]; then
  user_dir=${ICD_HOME:-$HOME/rprojects}
fi

if [ ! "$user_dir" = "" ]; then
  dir=$user_dir
else
  cp -r "${ICD_HOME:-$HOME/rprojects/icd}" "$icd_install_tmp"
fi

function finish {
  if [ "$clean" = yes ]; then
    rm -rf "$icd_install_tmp"
  else
    echo "leaving temp dir $icd_install_tmp"
  fi
  popd
}
trap finish EXIT
pushd "$dir"
install_dir="${icd_install_tmp}/install_icd"
mkdir "${install_dir}"
