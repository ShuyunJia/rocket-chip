package freechips.rocketchip.DRAMModel

import Chisel._

class DRAMModelToCtrlFSMIO()(implicit val conf: MemoryParameters) extends Bundle {
  val readAddr = UInt()
  val writeAddr = UInt()
  val doRead = Bool()
  val doWrite = Bool()
  val writeData = Vec(conf.deviceWidth, UInt(width = conf.memIF.dataBits / 4))
  val readData = new Bundle {
    val valid = Bool().flip()
    val data = Vec(conf.deviceWidth, UInt(width = conf.memIF.dataBits /4)).flip()
  }
  val fireTgtCycle = Bool().flip
}

class DRAMModelToMemControllerIO(implicit val conf:MemoryParameters) extends Bundle {
  val cmdBus = new DDRCommandBus().flip()
  val writeDataBus = new DDRDataBus().flip()
  val readDataBus = new DDRDataBus()
}

class DDRCommandBus(implicit val conf : MemoryParameters) extends Bundle {
  val valid = Bool()
  val cmd = UInt(width = conf.memConst.CMD_WIDTH)
  val bankAddr = UInt(width = conf.memConst.DRAM_BANK_ADDR_WIDTH)
  val rowAddr = UInt(width = conf.memConst.DRAM_ROW_ADDR_WIDTH)
  val colAddr = UInt(width = conf.memConst.DRAM_COL_ADDR_WIDTH)
}

class DDRDataBus(implicit val conf: MemoryParameters) extends Bundle{
  val valid = Bool()
  val data = Bits(width = conf.memConst.DATABUS_WIDTH)
}

class DRAMModelParameterIO(implicit val config : MemoryParameters) extends BankParameterIO {
  val tRRD = UInt(width = config.memConst.TIMING_COUNTER_WIDTH)
}

class DRAMModelErrorIO extends BankErrorIO {
  val tRRD_violation = Bool()
  val tWL_violation = Bool()
}

class DRAMTimingModel(implicit val conf: MemoryParameters) extends Module{
    val io = IO(new Bundle {
      val ctrlFSM = new DRAMModelToCtrlFSMIO()
      val memController = new DRAMModelToMemControllerIO()
      val params = new DRAMModelParameterIO().flip
      val errors = new DRAMModelErrorIO
    })
    val banks = Array.fill(conf.memConst.NUM_BANKS) {
      Module(new DRAMBank())
    }
    var tRRD_violation = (Bool(false))
    var invalidCmd = (Bool(false))
    var tRAS_violation = (Bool(false))
    var tRCD_violation = (Bool(false))
    var tRP_violation = (Bool(false))
    var tCCD_violation = (Bool(false))
    var tRTP_violation = (Bool(false))
    var tWTR_violation = (Bool(false))
    var tWR_violation = (Bool(false))

    for (i <- 0 until conf.memConst.NUM_BANKS) {
      banks(i).io.params <> io.params
      invalidCmd = invalidCmd || banks(i).io.errors.invalidCmd
      tRAS_violation = tRAS_violation || banks(i).io.errors.tRAS_violation
      tRCD_violation = tRCD_violation || banks(i).io.errors.tRCD_violation
      tRP_violation = tRP_violation || banks(i).io.errors.tRP_violation
      tCCD_violation = tCCD_violation || banks(i).io.errors.tCCD_violation
      tRTP_violation = tRTP_violation || banks(i).io.errors.tRTP_violation
      tWTR_violation = tWTR_violation || banks(i).io.errors.tWTR_violation
      tWR_violation = tWR_violation || banks(i).io.errors.tWR_violation
      banks(i).io.fireTgtCycle := io.ctrlFSM.fireTgtCycle
      banks(i).io.cmds.valid := io.memController.cmdBus.bankAddr === UInt(i) && io.memController.cmdBus.valid
      banks(i).io.cmds.activate := io.memController.cmdBus.cmd === conf.memConst.activate_cmd
      banks(i).io.cmds.read := io.memController.cmdBus.cmd === conf.memConst.read_cmd
      banks(i).io.cmds.write := io.memController.cmdBus.cmd === conf.memConst.write_cmd
      banks(i).io.cmds.precharge := io.memController.cmdBus.cmd === conf.memConst.precharge_cmd
      banks(i).io.cmds.bankAddr := UInt(i)
      banks(i).io.cmds.rowAddr := io.memController.cmdBus.rowAddr
      banks(i).io.cmds.colAddr := io.memController.cmdBus.colAddr
    }
    io.errors.invalidCmd := invalidCmd
    io.errors.tRAS_violation := tRAS_violation
    io.errors.tRCD_violation := tRCD_violation
    io.errors.tRP_violation := tRP_violation
    io.errors.tCCD_violation := tCCD_violation
    io.errors.tRTP_violation := tRTP_violation
    io.errors.tWTR_violation := tWTR_violation
    io.errors.tWR_violation := tWR_violation

    //select appropriate row addr to use based on bank selected
    val rowAddr = Wire(UInt())
    rowAddr := banks(0).io.ctrl.rowAddr
    for (i <- 0 until conf.memConst.NUM_BANKS) {
      when(io.memController.cmdBus.bankAddr === UInt(i)) {
        rowAddr := banks(i).io.ctrl.rowAddr
      }
    }

    //read delay chain
    io.ctrlFSM.doRead := io.memController.cmdBus.cmd === conf.memConst.read_cmd & io.memController.cmdBus.valid
    if (conf.addrOffsetWidth > 0) {
      io.ctrlFSM.readAddr := Cat(io.memController.cmdBus.bankAddr, rowAddr, io.memController.cmdBus.colAddr, UInt(0, width = conf.addrOffsetWidth))
    } else {
      io.ctrlFSM.readAddr := Cat(io.memController.cmdBus.bankAddr, rowAddr, io.memController.cmdBus.colAddr)
    }
  /*
    val readDataShiftRegs = Vec.fill(conf.memConst.tCL + conf.memConst.BURST_LENGTH - 1) {
      Reg(init = Bits(0, width = conf.memConst.DATABUS_WIDTH))
    }
    */
    val readDataShiftRegs = Reg(init = Vec.fill(conf.memConst.tCL + conf.memConst.BURST_LENGTH -1){UInt(0, width = conf.memConst.DATABUS_WIDTH)})
    val readDataValidShiftRegs = Reg(init = Vec.fill(conf.memConst.tCL + conf.memConst.BURST_LENGTH -1){Bool(false)})

    when(io.ctrlFSM.fireTgtCycle) {
      readDataValidShiftRegs(0) := Bool(false)
      for (i <- 1 until conf.memConst.tCL + conf.memConst.BURST_LENGTH - 1) {
        readDataShiftRegs(i) := readDataShiftRegs(i - 1)
        readDataValidShiftRegs(i) := readDataValidShiftRegs(i - 1)
      }
      var readData = io.ctrlFSM.readData.data(0)
      for (i <- 1 until conf.deviceWidth) {
        readData = Cat(io.ctrlFSM.readData.data(i), readData)
      }
      /*
      for (i <- 0 until conf.memConst.BURST_LENGTH) {
        when(io.ctrlFSM.readData.valid) {
          readDataShiftRegs(i) := readData((i + 1) * conf.memConst.DATABUS_WIDTH - 1, i * conf.memConst.DATABUS_WIDTH)
          readDataValidShiftRegs(i) := Bool(true)
        }
      }
      */
      when(io.ctrlFSM.readData.valid) {
         readDataShiftRegs(0) := readData(conf.memConst.DATABUS_WIDTH - 1, 0)
         readDataValidShiftRegs(0) := Bool(true)
      }
    }
    io.memController.readDataBus.valid := readDataValidShiftRegs(conf.memConst.tCL + conf.memConst.BURST_LENGTH - 2)

    io.memController.readDataBus.data := readDataShiftRegs(conf.memConst.tCL + conf.memConst.BURST_LENGTH - 2)

    //writeData delay chain
    val writeData = Reg(init = UInt(0, width = conf.memIF.dataBits))

    when(io.memController.writeDataBus.valid){
      writeData := io.memController.writeDataBus.data
    }
    if (conf.memConst.BURST_LENGTH - 1 > 0) {
      for (i <- 0 until 4) {
        io.ctrlFSM.writeData(i) := writeData((i + 1) * conf.memIF.dataBits /4 - 1, i * conf.memIF.dataBits/4)
      }
    } else {
      for (i <- 0 until conf.deviceWidth) {
        io.ctrlFSM.writeData(i) := io.memController.writeDataBus.data((i + 1) * conf.memIF.dataBits - 1, i * conf.memIF.dataBits)
      }
    }
    //doWrite and writeAddr delay chain
    val expectReceiveWriteData = Reg(init = Bool(false))

    val doWriteAddr = Wire(UInt(conf.memIF.addrBits))
    val doWrite = io.memController.cmdBus.cmd === conf.memConst.write_cmd & io.memController.cmdBus.valid

    if(conf.addrOffsetWidth >0){
      doWriteAddr := Cat(io.memController.cmdBus.bankAddr, rowAddr, io.memController.cmdBus.colAddr, UInt(0, width = conf.addrOffsetWidth))
    } else{
      doWriteAddr := Cat(io.memController.cmdBus.bankAddr, rowAddr, io.memController.cmdBus.colAddr)
    }
      val doWriteAddrTwl = ShiftRegister(doWriteAddr, conf.memConst.tWL -1 )
      val doWriteTwl = ShiftRegister(doWrite, conf.memConst.tWL -1 )

      io.ctrlFSM.doWrite := doWriteTwl
      io.ctrlFSM.writeAddr := doWriteAddrTwl

      when(io.ctrlFSM.fireTgtCycle & io.memController.cmdBus.valid){
        expectReceiveWriteData := Bool(true)
      }.elsewhen(io.memController.writeDataBus.valid){
        expectReceiveWriteData := Bool(false)
      }

    val tWL_violation = Reg(init = Bool(false))
    when(io.ctrlFSM.fireTgtCycle) {
      tWL_violation := (doWriteTwl &&  ~io.memController.writeDataBus.valid) | tWL_violation
    }
    io.errors.tWL_violation := tWL_violation
}
