package freechips.rocketchip.DRAMModel

import Chisel._


class MemController()(implicit val conf: MemoryParameters) extends Module {
  val io= IO(new Bundle {
    val mem_cmd_queue = new DecoupledIO(new MemReqCmd()).flip()
    val mem_data_queue = new DecoupledIO(new MemData()).flip()
    val mem_resp_queue = new DecoupledIO(new MemResp())
    val DRAMModel = new DRAMModelToMemControllerIO().flip()
    val fireTgtCycle = Bool(INPUT)
    val params = new DRAMModelParameterIO().flip()
    //val debug_out = Vec.fill(conf.deviceWidth){new MemData}.asOutput
    //val debug_readData = Bits().asOutput
  })

  //Predef.assert(conf.deviceWidth == 4)
  //Predef.assert(memConst.DRAM_BANK_ADDR_WIDTH +conf.memConst.DRAM_ROW_ADDR_WIDTH +conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth <= mif.addrBits)

  val timer = Module(new MemControllerTimer())
  timer.io.params <> io.params
  timer.io.fireTgtCycle := io.fireTgtCycle
  timer.io.cmds.valid := io.DRAMModel.cmdBus.valid
  timer.io.cmds.activate := io.DRAMModel.cmdBus.cmd ===conf.memConst.activate_cmd
  timer.io.cmds.read := io.DRAMModel.cmdBus.cmd ===conf.memConst.read_cmd
  timer.io.cmds.write := io.DRAMModel.cmdBus.cmd ===conf.memConst.write_cmd
  timer.io.cmds.precharge := io.DRAMModel.cmdBus.cmd ===conf.memConst.precharge_cmd
  timer.io.cmds.bankAddr := io.DRAMModel.cmdBus.bankAddr
  timer.io.cmds.rowAddr := io.DRAMModel.cmdBus.rowAddr
  timer.io.cmds.colAddr := io.DRAMModel.cmdBus.colAddr

  val currentAddr = Reg(init = UInt(0, conf.memIF.addrBits))
  val nextAddr = Wire(UInt());
  nextAddr := currentAddr
  when(io.fireTgtCycle){
    currentAddr := nextAddr
  }

  val readData = Reg(init = UInt(0, width = conf.memConst.DATABUS_WIDTH))
  val readDataValid = Reg(init = Bool(false))
  readDataValid := io.DRAMModel.readDataBus.valid
  readData := io.DRAMModel.readDataBus.data

  //read data handler(remember to check the order of the memdatas sent and received)
  val rHIdle :: rHEnq1 :: rHEnq2 :: rHEnq3 :: Nil = Enum(UInt(), 4)
  val rHCurrentState = Reg(init = rHIdle)
  val rHNextState = Wire(UInt()); rHNextState := rHCurrentState

  when(io.fireTgtCycle){
    rHCurrentState := rHNextState
  }

  val cmd = Queue(io.mem_cmd_queue, 16)
  cmd.ready := Bool(false)

  val readTag = Wire(Decoupled(cmd.bits.tag))
  readTag.valid := cmd.fire() && ~cmd.bits.rw
  readTag.bits := cmd.bits.tag
  val readTagResp = Queue(readTag,16)
  readTagResp.ready := io.mem_resp_queue.fire() && io.fireTgtCycle

  val writeDataQueue = Queue(io.mem_data_queue)
  writeDataQueue.ready := Bool(false)

  io.mem_resp_queue.valid := readDataValid
  io.mem_resp_queue.bits.tag := readTagResp.bits
  io.mem_resp_queue.bits.data := readData

/*
  when(rHCurrentState === rHIdle){
    when(readDataQueues(0).io.deq.valid && io.mem_resp_queue.ready){
      io.mem_resp_queue.valid := Bool(true) & io.fireTgtCycle
      io.mem_resp_queue.bits.data := readDataQueues(0).io.deq.bits.data
      rHNextState := rHEnq1
    }
  }.elsewhen(rHCurrentState === rHEnq1){
    when(io.mem_resp_queue.ready){
      io.mem_resp_queue.valid := Bool(true) & io.fireTgtCycle
      io.mem_resp_queue.bits.data := readDataQueues(1).io.deq.bits.data
      rHNextState := rHEnq2
    }
  }.elsewhen(rHCurrentState === rHEnq2){
    when(io.mem_resp_queue.ready){
      io.mem_resp_queue.valid := Bool(true) & io.fireTgtCycle
      io.mem_resp_queue.bits.data := readDataQueues(2).io.deq.bits.data
      rHNextState := rHEnq3
    }
  }.elsewhen(rHCurrentState === rHEnq3){
    when(io.mem_resp_queue.ready){
      io.mem_resp_queue.valid := Bool(true) & io.fireTgtCycle
      io.mem_resp_queue.bits.data := readDataQueues(3).io.deq.bits.data
      readTagQueue.io.deq.ready := Bool(true) & io.fireTgtCycle
      for(i <- 0 until conf.deviceWidth){
        readDataQueues(i).io.deq.ready := Bool(true) & io.fireTgtCycle
      }
      rHNextState := rHIdle
    }
  }
  */
  //write data delay chain
  val dataInBusValid = Wire(Bool())
  val dataInBusData = Wire(UInt(width = conf.memIF.dataBits))

  val dataInBusValidTwl = ShiftRegister(dataInBusValid, conf.memConst.tWL - 1)
  val dataInBusDataTwl = ShiftRegister(dataInBusData, conf.memConst.tWL - 1)

  io.DRAMModel.writeDataBus.valid := dataInBusValidTwl
  io.DRAMModel.writeDataBus.data := dataInBusDataTwl

  //control fsm
  val idle :: readRowOpened :: colOpened :: writeRowOpened :: writeData1 :: writeData2 :: writeData3 :: Nil = Enum(UInt(), 7)
  val currentState = Reg(init = idle)
  val nextState = Wire(UInt()); nextState := currentState
  when(io.fireTgtCycle){
    currentState := nextState
  }

  io.DRAMModel.cmdBus.valid := Bool(false)
  io.DRAMModel.cmdBus.cmd :=conf.memConst.activate_cmd
  io.DRAMModel.cmdBus.bankAddr := UInt(0)
  io.DRAMModel.cmdBus.rowAddr := UInt(0)
  io.DRAMModel.cmdBus.colAddr := UInt(0)
  dataInBusValid := Bool(false)
  val writeDatas = Reg(init = {Wire(new MemData)})

  when(currentState === idle){
    when(cmd.valid & readTag.ready){
      when(timer.io.activate_rdy){
        //deque cmd queue
        cmd.ready := Bool(true) & io.fireTgtCycle
        //store addr
        nextAddr := cmd.bits.addr
        //set fsm outputs
        io.DRAMModel.cmdBus.valid := Bool(true)
        io.DRAMModel.cmdBus.cmd := conf.memConst.activate_cmd
        io.DRAMModel.cmdBus.rowAddr := cmd.bits.addr(conf.memConst.DRAM_ROW_ADDR_WIDTH + conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth - 1, conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth)
        io.DRAMModel.cmdBus.bankAddr := cmd.bits.addr(conf.memConst.DRAM_BANK_ADDR_WIDTH + conf.memConst.DRAM_ROW_ADDR_WIDTH + conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth - 1, conf.memConst.DRAM_ROW_ADDR_WIDTH + conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth)
        when(cmd.bits.rw){
          nextState := writeRowOpened
        }.otherwise{
          nextState := readRowOpened
        }
      }
    }
  }.elsewhen(currentState === readRowOpened){
    when(timer.io.read_rdy){
      io.DRAMModel.cmdBus.valid := Bool(true)
      io.DRAMModel.cmdBus.cmd := conf.memConst.read_cmd
      io.DRAMModel.cmdBus.bankAddr := currentAddr(conf.memConst.DRAM_BANK_ADDR_WIDTH + conf.memConst.DRAM_ROW_ADDR_WIDTH + conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth - 1, conf.memConst.DRAM_ROW_ADDR_WIDTH + conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth)
      io.DRAMModel.cmdBus.colAddr := currentAddr(conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth - 1, conf.addrOffsetWidth)
      nextState := colOpened
    }
  }.elsewhen(currentState === colOpened){
    when(timer.io.precharge_rdy){
      io.DRAMModel.cmdBus.valid := Bool(true)
      io.DRAMModel.cmdBus.cmd := conf.memConst.precharge_cmd
      io.DRAMModel.cmdBus.rowAddr := currentAddr(conf.memConst.DRAM_ROW_ADDR_WIDTH + conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth - 1, conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth)
      io.DRAMModel.cmdBus.bankAddr := currentAddr(conf.memConst.DRAM_BANK_ADDR_WIDTH + conf.memConst.DRAM_ROW_ADDR_WIDTH + conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth - 1, conf.memConst.DRAM_ROW_ADDR_WIDTH +conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth)
      nextState := idle
    }
  }.elsewhen(currentState === writeRowOpened) {
    when(writeDataQueue.valid && timer.io.write_rdy) {
      writeDataQueue.ready := Bool(true) & io.fireTgtCycle
      dataInBusData := writeDataQueue.bits.data
      dataInBusValid := Bool(true)
      io.DRAMModel.cmdBus.valid := Bool(true)
      io.DRAMModel.cmdBus.cmd := conf.memConst.write_cmd
      io.DRAMModel.cmdBus.bankAddr := currentAddr(conf.memConst.DRAM_BANK_ADDR_WIDTH + conf.memConst.DRAM_ROW_ADDR_WIDTH + conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth - 1, conf.memConst.DRAM_ROW_ADDR_WIDTH + conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth)
      io.DRAMModel.cmdBus.colAddr := currentAddr(conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth - 1, conf.addrOffsetWidth)
      nextState := colOpened
    }
  }.elsewhen(currentState === colOpened){
    when(timer.io.precharge_rdy){
      io.DRAMModel.cmdBus.valid := Bool(true)
      io.DRAMModel.cmdBus.cmd :=conf.memConst.precharge_cmd
      io.DRAMModel.cmdBus.bankAddr := currentAddr(conf.memConst.DRAM_BANK_ADDR_WIDTH +conf.memConst.DRAM_ROW_ADDR_WIDTH +conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth - 1,conf.memConst.DRAM_ROW_ADDR_WIDTH +conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth)
      io.DRAMModel.cmdBus.colAddr := currentAddr(conf.memConst.DRAM_COL_ADDR_WIDTH + conf.addrOffsetWidth - 1, conf.addrOffsetWidth)
      nextState := idle
    }
  }
}
