#!/usr/bin/env bash
# ==============================================================================
# CalculiX & PrePoMax Result File Explorer & Interactive Viewer Launcher
# ==============================================================================
# Function:
#   1. Crawls the current directory (or specified path) and all subdirectories
#      for CalculiX result files (*.frd, *.fbi, *.pmx).
#   2. Displays an interactive indexed list with timestamps and file sizes.
#   3. Supports instant keyword filtering to quickly find benchmark results.
#   4. Prompts the user to select a file and choose the viewer:
#      - CGX (CalculiX GraphiX with OpenGL mesa software safety)
#      - CGX High-Contrast Beam Mode (with white background & thick beam outlines)
#      - PrePoMax (via /home/pierre/prepomax/run_wine.sh)
#      - Companion demo launcher script (if present in the folder)
# ==============================================================================

# ANSI Color codes
BOLD="\033[1m"
GREEN="\033[0;32m"
CYAN="\033[0;36m"
YELLOW="\033[1;33m"
BLUE="\033[0;34m"
MAGENTA="\033[0;35m"
RED="\033[0;31m"
DIM="\033[2m"
RESET="\033[0m"

# Default Binary Paths
CGX_BIN="$(which cgx 2>/dev/null || echo "")"
if [ -z "$CGX_BIN" ] || [ ! -x "$CGX_BIN" ]; then
    for cand in "/home/pierre/CCX-CB/cgx_2.23.all/CalculiX/cgx_2.23/src/cgx" "/usr/local/bin/cgx" "/usr/bin/cgx"; do
        if [ -x "$cand" ]; then
            CGX_BIN="$cand"
            break
        fi
    done
fi

PREPOMAX_SCRIPT=""
for pmx_cand in "$HOME/prepomax/run_wine.sh" "/home/pierre/prepomax/run_wine.sh"; do
    if [ -f "$pmx_cand" ]; then
        PREPOMAX_SCRIPT="$pmx_cand"
        break
    fi
done

# Starting Directory (default to current directory or CCX-CB root)
START_DIR="${1:-$(pwd)}"
if [ ! -d "$START_DIR" ]; then
    START_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
fi

export __GLX_VENDOR_LIBRARY_NAME=mesa
export LIBGL_ALWAYS_SOFTWARE=1

# Print Header
print_header() {
    clear 2>/dev/null || echo ""
    echo -e "${BOLD}${BLUE}================================================================================${RESET}"
    echo -e "${BOLD}${CYAN}   CalculiX & PrePoMax Result Explorer (FRD / PMX Crawler)   ${RESET}"
    echo -e "${BOLD}${BLUE}================================================================================${RESET}"
    echo -e " ${DIM}Root Search Directory:${RESET} ${BOLD}${START_DIR}${RESET}"
    echo -e " ${DIM}Available Viewers    :${RESET} ${GREEN}CGX${RESET} (${CGX_BIN:-Not Found}), ${MAGENTA}PrePoMax Wine${RESET} (${PREPOMAX_SCRIPT})"
    echo -e "${BOLD}${BLUE}================================================================================${RESET}"
}

# Crawl files matching extensions
crawl_files() {
    local filter="$1"
    local search_path="$START_DIR"
    
    # Find all .frd and .pmx files, excluding .git, __pycache__, .objs, etc.
    if [ -n "$filter" ]; then
        find "$search_path" -type f \( -name "*.frd" -o -name "*.pmx" \) \
            ! -path "*/.git/*" ! -path "*/__pycache__/*" ! -path "*/.objs/*" ! -path "*/.dep/*" \
            | grep -i "$filter" | sort
    else
        find "$search_path" -type f \( -name "*.frd" -o -name "*.pmx" \) \
            ! -path "*/.git/*" ! -path "*/__pycache__/*" ! -path "*/.objs/*" ! -path "*/.dep/*" \
            | sort
    fi
}

# Open file with CGX Standard
open_cgx_standard() {
    local file="$1"
    local dir="$(dirname "$file")"
    local base="$(basename "$file")"
    
    if [ -z "$CGX_BIN" ] || [ ! -x "$CGX_BIN" ]; then
        echo -e "${RED}Error: CGX binary not found at $CGX_BIN${RESET}"
        return 1
    fi
    
    echo -e "\n${GREEN}==>${RESET} Launching ${BOLD}$base${RESET} in CGX Standard Mode..."
    (
        cd "$dir" || exit 1
        export __GLX_VENDOR_LIBRARY_NAME=mesa
        export LIBGL_ALWAYS_SOFTWARE=1
        if [[ "$base" == *.fbi ]]; then
            "$CGX_BIN" -b "$base" > /tmp/cgx.log 2>&1
        elif [[ "$base" == *.frd ]]; then
            "$CGX_BIN" "$base" > /tmp/cgx.log 2>&1
        else
            "$CGX_BIN" -a "$base" > /tmp/cgx.log 2>&1
        fi
    ) &
    disown $! 2>/dev/null || true
}

# Open file with CGX High-Contrast Mode (for 1D beams + 3D solids)
open_cgx_high_contrast() {
    local file="$1"
    local dir="$(dirname "$file")"
    local base="$(basename "$file")"
    local jobname="${base%.*}"
    
    if [ -z "$CGX_BIN" ] || [ ! -x "$CGX_BIN" ]; then
        echo -e "${RED}Error: CGX binary not found at $CGX_BIN${RESET}"
        return 1
    fi
    
    # Check if a companion .inp exists
    local inp_arg=""
    if [ -f "$dir/$jobname.inp" ]; then
        inp_arg="read $jobname.inp nom"
    fi
    
    local tmp_fbi="$dir/.tmp_view_${jobname}.fbi"
    cat <<EOF > "$tmp_fbi"
read $base
$inp_arg
view bg w
view fill
view sh off
view ill off
view elem
view rot x -30
view rot y 35
view rot z 15
view zoom 1.2
view disp
scal d 1.0
plot fv all
plus ev all 8
EOF

    echo -e "\n${GREEN}==>${RESET} Launching ${BOLD}$base${RESET} in CGX High-Contrast Beam Mode..."
    (
        cd "$dir" || exit 1
        export __GLX_VENDOR_LIBRARY_NAME=mesa
        export LIBGL_ALWAYS_SOFTWARE=1
        "$CGX_BIN" -b ".tmp_view_${jobname}.fbi" > /tmp/cgx.log 2>&1
        rm -f ".tmp_view_${jobname}.fbi"
    ) &
    disown $! 2>/dev/null || true
}

# Open file with PrePoMax via Wine
open_prepomax_wine() {
    local file="$1"
    local abs_path="$(realpath "$file")"
    
    if [ ! -f "$PREPOMAX_SCRIPT" ]; then
        echo -e "${RED}Error: PrePoMax Wine launcher script not found at $PREPOMAX_SCRIPT${RESET}"
        return 1
    fi
    
    echo -e "\n${MAGENTA}==>${RESET} Launching ${BOLD}$(basename "$file")${RESET} in PrePoMax (Wine)..."
    (
        export WINEDEBUG=-all
        export __GLX_VENDOR_LIBRARY_NAME=mesa
        export LIBGL_ALWAYS_SOFTWARE=1
        "$PREPOMAX_SCRIPT" "$abs_path" > /tmp/prepomax_wine.log 2>&1
    ) &
    disown $! 2>/dev/null || true
}

# Main Interactive Loop
main() {
    local current_filter=""
    
    while true; do
        print_header
        
        # Read matching files into an array
        local file_list=()
        while IFS= read -r line; do
            [ -n "$line" ] && file_list+=("$line")
        done < <(crawl_files "$current_filter")
        
        local total_files=${#file_list[@]}
        
        if [ -n "$current_filter" ]; then
            echo -e " ${YELLOW}Active Search Filter:${RESET} '${BOLD}${current_filter}${RESET}' (${total_files} matches found)"
            echo -e " ${DIM}(Type '/clear' or '/' to reset filter)${RESET}"
        else
            echo -e " ${DIM}Found ${BOLD}${total_files}${RESET}${DIM} result files across folders & subfolders.${RESET}"
        fi
        echo -e "${BOLD}${BLUE}--------------------------------------------------------------------------------${RESET}"
        
        if [ $total_files -eq 0 ]; then
            echo -e " ${RED}No matching result files (*.frd, *.fbi, *.pmx) found.${RESET}"
            echo -e "\n Options:"
            echo -e "   ${BOLD}/${RESET}         - Clear filter"
            echo -e "   ${BOLD}d <path>${RESET} - Change search directory"
            echo -e "   ${BOLD}q${RESET}        - Quit"
            echo ""
            read -r -p " Enter command: " choice
            case "$choice" in
                q|Q|exit) exit 0 ;;
                /|/clear) current_filter="" ;;
                d\ *) START_DIR="${choice#d }" ;;
                *) current_filter="$choice" ;;
            esac
            continue
        fi
        
        # Display up to 35 items at a time
        local max_display=30
        local count=0
        for i in "${!file_list[@]}"; do
            local idx=$((i + 1))
            local fpath="${file_list[$i]}"
            local relpath="${fpath#$START_DIR/}"
            local fname="$(basename "$fpath")"
            local fdir="$(dirname "$relpath")"
            local fsize=$(ls -lh "$fpath" | awk '{print $5}')
            local ftime=$(date -r "$fpath" +"%Y-%m-%d %H:%M")
            
            # Color code based on extension
            local ext_color="$CYAN"
            if [[ "$fname" == *.frd ]]; then
                ext_color="$GREEN"
            elif [[ "$fname" == *.fbi ]]; then
                ext_color="$YELLOW"
            elif [[ "$fname" == *.pmx ]]; then
                ext_color="$MAGENTA"
            fi
            
            printf "  ${BOLD}%3d)${RESET} %-16s ${DIM}%6s${RESET}  ${ext_color}%-32s${RESET} ${DIM}(%s)${RESET}\n" \
                "$idx" "$ftime" "$fsize" "$fname" "$fdir"
                
            count=$((count + 1))
            if [ $count -ge $max_display ] && [ $total_files -gt $max_display ]; then
                local remaining=$((total_files - max_display))
                echo -e "  ${DIM}... and ${remaining} more files. Use filter (e.g. '/<keyword>') to narrow down.${RESET}"
                break
            fi
        done
        
        echo -e "${BOLD}${BLUE}--------------------------------------------------------------------------------${RESET}"
        echo -e " ${BOLD}Actions:${RESET}"
        echo -e "   ${GREEN}<number>${RESET}       - Select file to open"
        echo -e "   ${YELLOW}/<text>${RESET}        - Filter results by keyword (e.g. ${CYAN}/modal${RESET}, ${CYAN}/dynamic${RESET}, ${CYAN}/thermal${RESET})"
        echo -e "   ${YELLOW}/clear${RESET}         - Reset filter"
        echo -e "   ${BLUE}d <path>${RESET}       - Change search directory"
        echo -e "   ${RED}q${RESET}              - Quit"
        echo -e "${BOLD}${BLUE}================================================================================${RESET}"
        
        read -r -p " Select file number or command: " user_input
        
        case "$user_input" in
            q|Q|exit)
                echo -e "${GREEN}Goodbye!${RESET}"
                exit 0
                ;;
            /clear|/)
                current_filter=""
                ;;
            /*)
                current_filter="${user_input#/}"
                ;;
            d\ *)
                new_dir="${user_input#d }"
                if [ -d "$new_dir" ]; then
                    START_DIR="$(realpath "$new_dir")"
                    current_filter=""
                else
                    echo -e "${RED}Directory does not exist: $new_dir${RESET}"
                    sleep 1.5
                fi
                ;;
            ''|*[!0-9]*)
                # If non-numeric input was given without '/', treat as filter
                if [ -n "$user_input" ]; then
                    current_filter="$user_input"
                fi
                ;;
            *)
                # Valid numeric selection
                local sel_idx=$((user_input - 1))
                if [ $sel_idx -ge 0 ] && [ $sel_idx -lt $total_files ]; then
                    local selected_file="${file_list[$sel_idx]}"
                    local sel_dir="$(dirname "$selected_file")"
                    local sel_base="$(basename "$selected_file")"
                    
                    # Check for companion launcher scripts (.sh or .fbi) in the same directory
                    local companion_sh=""
                    for sh_cand in "$sel_dir"/view_*.sh "$sel_dir"/*.sh; do
                        if [ -f "$sh_cand" ] && [ -x "$sh_cand" ]; then
                            companion_sh="$sh_cand"
                            break
                        fi
                    done
                    
                    echo -e "\n${BOLD}${BLUE}--------------------------------------------------------------------------------${RESET}"
                    echo -e " Selected File: ${BOLD}${GREEN}$selected_file${RESET}"
                    echo -e "${BOLD}${BLUE}--------------------------------------------------------------------------------${RESET}"
                    echo -e " Choose Postprocessor / Viewer:"
                    echo -e "   ${BOLD}1)${RESET} ${GREEN}CGX (CalculiX GraphiX Standard Mode)${RESET}"
                    echo -e "   ${BOLD}2)${RESET} ${CYAN}CGX (High-Contrast Beam Visualization Mode)${RESET}"
                    echo -e "   ${BOLD}3)${RESET} ${MAGENTA}PrePoMax (via Wine - /home/pierre/prepomax/run_wine.sh)${RESET}"
                    if [ -n "$companion_sh" ]; then
                        echo -e "   ${BOLD}4)${RESET} ${YELLOW}Companion Demo Launcher ($(basename "$companion_sh"))${RESET}"
                    fi
                    echo -e "   ${BOLD}b)${RESET} Back to file list"
                    echo -e "${BOLD}${BLUE}--------------------------------------------------------------------------------${RESET}"
                    
                    read -r -p " Enter viewer choice [1-4 or b]: " viewer_choice
                    case "$viewer_choice" in
                        1)
                            open_cgx_standard "$selected_file"
                            sleep 1
                            ;;
                        2)
                            open_cgx_high_contrast "$selected_file"
                            sleep 1
                            ;;
                        3)
                            open_prepomax_wine "$selected_file"
                            sleep 1
                            ;;
                        4)
                            if [ -n "$companion_sh" ]; then
                                echo -e "\n${YELLOW}==>${RESET} Executing companion script ${BOLD}$(basename "$companion_sh")${RESET}..."
                                ( cd "$sel_dir" && "$companion_sh" > /tmp/companion_sh.log 2>&1 ) &
                                disown $! 2>/dev/null || true
                                sleep 1
                            else
                                echo -e "${RED}Invalid selection.${RESET}"
                                sleep 1
                            fi
                            ;;
                        b|B)
                            ;;
                        *)
                            echo -e "${RED}Invalid selection.${RESET}"
                            sleep 1
                            ;;
                    esac
                else
                    echo -e "${RED}Invalid file number: $user_input${RESET}"
                    sleep 1.2
                fi
                ;;
        esac
    done
}

# Run main loop
main "$@"
