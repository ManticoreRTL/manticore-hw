#!/bin/bash -x

alias source=.

init_xilinx() {
  xversion="2022.1"
  echo "Initializing Xilinx ${xversion}"
  source /scratch/softs/Xilinx/Vivado/${xversion}/settings64.sh
  export XILINX_XRT=/opt/xilinx/xrt
  export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${XILINX_XRT}/lib
  export PATH=${PATH}:${XILINX_XRT}/bin
}

# $1 = DIMX (int)
# $2 = DIMY (int)
# $3 = PLATFORM (str)
# $4 = FREQ (int)
# $5 = EN_CFU (bool)
# $6 = N_HOP (int)
# $7 = PLACEMENT_ALGORITHM (str1,str2,...)
# $8 = IMPL_STRATEGIES (str1,str2,...)
# $9 = BUILD_DIR (str)
launch(){
  DIMX=$1
  DIMY=$2
  PLATFORM=$3
  FREQ=$4
  EN_CFU=$5
  N_HOP=$6
  PLACEMENT_ALGORITHM=$7
  IMPL_STRATEGIES=$8
  BUILD_DIR=$9

  echo "PWD: `pwd`"
  echo "Launching build ${DIMX}x${DIMY} in ${BUILD_DIR}\n"

  git_rev=`git rev-parse HEAD`
  vivado -version > ${BUILD_DIR}/vivado_version

  build_info="${BUILD_DIR}/build_info"
  echo "version: ${git_rev}" > ${build_info}
  echo "XILINX_XRT: ${XILINX_XRT}" >> ${build_info}
  echo "XILINX_VIVAOD: ${XILINX_VIVADO}" >> ${build_info}
  echo "topology: ${DIMX}x${DIMY}" >> ${build_info}
  echo "build start: `date`" >> ${build_info}

  args="-t hw -x ${DIMX} -y ${DIMY} -o ${BUILD_DIR} -p ${PLATFORM} -f ${FREQ} --enable_custom_alu ${EN_CFU} --n_hop ${N_HOP} --strategies ${IMPL_STRATEGIES}"
  echo "args: ${args}" >> ${build_info}

  sbt clean

  pblocks="${BUILD_DIR}/pblocks.tcl"
  sbt -J-Xmx8192m "runMain manticore.machine.Main placement --platform ${PLATFORM} --dimx ${DIMX} --dimy ${DIMY} --algorithm ${PLACEMENT_ALGORITHM} --output ${pblocks}"
  sbt -J-Xmx8192m "runMain manticore.machine.Main ${args} -P ${pblocks}"

  echo "build end: `date`" >> ${build_info}

  echo "Compressing build artifcats"
  tar -czf ${BUILD_DIR}/artifacts_${DIMX}x${DIMY}_${FREQ}.tar.gz \
    "${build_info}" \
    "${BUILD_DIR}/vivado_version" \
    "${BUILD_DIR}/bin/*.xclbin" \
    "${BUILD_DIR}/bin/_x/reports" \
    "${BUILD_DIR}/hdl" \
    "${pblocks}"

  echo "Done!"
}

DIMX=$1
DIMY=$2
PLATFORM=$3
FREQ=$4
EN_CFU=$5
N_HOP=$6
PLACEMENT_ALGORITHM=$7
IMPL_STRATEGIES=$8
BUILD_DIR=$9

init_xilinx
mkdir -p ${BUILD_DIR}
launch ${DIMX} ${DIMY} ${PLATFORM} ${FREQ} ${EN_CFU} ${N_HOP} ${PLACEMENT_ALGORITHM} ${IMPL_STRATEGIES} ${BUILD_DIR}
