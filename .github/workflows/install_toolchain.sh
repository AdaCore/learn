#!/bin/bash -eux

GNAT_FSF_BUILDS=https://github.com/alire-project/GNAT-FSF-builds/releases/download

TOOLCHAIN_CONFIG=${GITHUB_WORKSPACE}/frontend/py_modules/code_projects/toolchain.ini

INSTALL_GNATPROVE=NO
INSTALL_GNAT=NO
INSTALL_GPRBUILD=NO

for i in "$@"; do
  case $i in
    --gnatprove)
      INSTALL_GNATPROVE=YES
      shift # past argument with no value
      ;;
    --gnat)
      INSTALL_GNAT=YES
      shift # past argument with no value
      ;;
    --gprbuild)
      INSTALL_GPRBUILD=YES
      shift # past argument with no value
      ;;
    -*|--*)
      echo "Unknown option $i"
      exit 1
      ;;
    *)
      ;;
  esac
done

path_ada_toolchain_root=$(crudini --get $TOOLCHAIN_CONFIG toolchain_path root)
path_ada_toolchain_default=$(crudini --get $TOOLCHAIN_CONFIG toolchain_path default)
path_ada_toolchain_selected=$(crudini --get $TOOLCHAIN_CONFIG toolchain_path selected)
default_version_gnat=$(crudini --get $TOOLCHAIN_CONFIG default_version gnat)
default_version_gnatprove=$(crudini --get $TOOLCHAIN_CONFIG default_version gnatprove)
default_version_gprbuild=$(crudini --get $TOOLCHAIN_CONFIG default_version gprbuild)
toolchain_versions_gnat=$(crudini --get $TOOLCHAIN_CONFIG toolchains gnat)
toolchain_versions_gnatprove=$(crudini --get $TOOLCHAIN_CONFIG toolchains gnatprove)
toolchain_versions_gprbuild=$(crudini --get $TOOLCHAIN_CONFIG toolchains gprbuild)

echo path_ada_toolchain_root:      $path_ada_toolchain_root
echo path_ada_toolchain_default:   $path_ada_toolchain_default
echo path_ada_toolchain_selected:  $path_ada_toolchain_selected
echo default_version_gnat:         $default_version_gnat
echo default_version_gnatprove:    $default_version_gnatprove
echo default_version_gprbuild:     $default_version_gprbuild
echo toolchain_versions_gnat:      $toolchain_versions_gnat
echo toolchain_versions_gnatprove  $toolchain_versions_gnatprove
echo toolchain_versions_gprbuild   $toolchain_versions_gprbuild

sudo mkdir -p ${path_ada_toolchain_root}
sudo mkdir -p ${path_ada_toolchain_default}
sudo mkdir -p ${path_ada_toolchain_selected}


if [ "$INSTALL_GNAT" == "YES" ]; then
    gnat_version=(${toolchain_versions_gnat})
    sudo mkdir ${path_ada_toolchain_root}/gnat

    for tool_version in ${gnat_version[@]}; do
        echo Installing GNAT $tool_version
        sudo wget -O gnat.tar.gz ${GNAT_FSF_BUILDS}/gnat-${tool_version}/gnat-x86_64-linux-${tool_version}.tar.gz && \
        sudo tar xzf gnat.tar.gz && \
        sudo mv gnat-* ${path_ada_toolchain_root}/gnat/${tool_version} && \
        sudo rm *.tar.gz
    done

    sudo echo "${path_ada_toolchain_default}/gnat/bin"  >> $GITHUB_PATH
    sudo echo "${path_ada_toolchain_selected}/gnat/bin" >> $GITHUB_PATH

    sudo ln -sf ${path_ada_toolchain_root}/gnat/${default_version_gnat}           ${path_ada_toolchain_default}/gnat
fi

if [ "$INSTALL_GNATPROVE" == "YES" ]; then
    gnat_prove_version=(${toolchain_versions_gnatprove})
    sudo mkdir ${path_ada_toolchain_root}/gnatprove
    for tool_version in ${gnat_prove_version[@]}; do
        echo Installing GNATprove $tool_version
        sudo wget -O gnatprove.tar.gz ${GNAT_FSF_BUILDS}/gnatprove-${tool_version}/gnatprove-x86_64-linux-${tool_version}.tar.gz && \
        sudo tar xzf gnatprove.tar.gz && \
        sudo mv gnatprove-* ${path_ada_toolchain_root}/gnatprove/${tool_version} && \
        sudo rm *.tar.gz
    done

    sudo echo "${path_ada_toolchain_default}/gnatprove/bin"  >> $GITHUB_PATH
    sudo echo "${path_ada_toolchain_selected}/gnatprove/bin" >> $GITHUB_PATH

    sudo ln -sf ${path_ada_toolchain_root}/gnatprove/${default_version_gnatprove} ${path_ada_toolchain_default}/gnatprove
fi

if [ "$INSTALL_GPRBUILD" == "YES" ]; then
    gprbuild_version=(${toolchain_versions_gprbuild})
    sudo mkdir ${path_ada_toolchain_root}/gprbuild
    for tool_version in ${gprbuild_version[@]}; do
        echo Installing GPRbuild $tool_version
        sudo wget -O gprbuild.tar.gz ${GNAT_FSF_BUILDS}/gprbuild-${tool_version}/gprbuild-x86_64-linux-${tool_version}.tar.gz && \
        sudo tar xzf gprbuild.tar.gz && \
        sudo mv gprbuild-* ${path_ada_toolchain_root}/gprbuild/${tool_version} && \
        sudo rm *.tar.gz
    done

    sudo echo "${path_ada_toolchain_default}/gprbuild/bin"  >> $GITHUB_PATH
    sudo echo "${path_ada_toolchain_selected}/gprbuild/bin" >> $GITHUB_PATH

    sudo ln -sf ${path_ada_toolchain_root}/gprbuild/${default_version_gprbuild}   ${path_ada_toolchain_default}/gprbuild
fi

sudo chmod -R ugo+w ${path_ada_toolchain_selected}
