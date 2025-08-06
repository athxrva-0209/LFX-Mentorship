package stack

import chisel3.stage.ChiselStage
import java.nio.file.Paths

// Your code starts here
import chisel3._
import chisel3.util._

class StackModule(val dataWidth: Int, val len: Int) extends Module {
  val io = IO(new Bundle {
    val in        = Input(UInt(32.W))
    val out       = Output(UInt(dataWidth.W))
    val underflow = Output(Bool())
    val overflow  = Output(Bool())
    val popped    = Output(Bool())
    val peeked    = Output(Bool())
    val isEmpty   = Output(Bool())
    val isFull    = Output(Bool())
  })

  val opcode  = io.in(6, 0)             
  val data = io.in(31, 7)     
  val payload = if (dataWidth > 25) Cat(0.U((dataWidth-25).W), data) else data(dataWidth-1, 0)    

  val stack = RegInit(VecInit(Seq.fill(len)(0.U(dataWidth.W))))
  val sp    = RegInit(0.U(log2Ceil(len + 1).W)) 

  val underflowReg = RegInit(false.B)
  val overflowReg = RegInit(false.B)
  val poppedReg = RegInit(false.B)
  val peekedReg = RegInit(false.B)
  val outReg = RegInit(0.U(dataWidth.W))
  io.out := outReg

  // Default outputs every cycle
  underflowReg := false.B
  overflowReg  := false.B
  poppedReg    := false.B
  peekedReg    := false.B
  outReg       := 0.U
  io.isEmpty   := (sp === 0.U)
  io.isFull    := (sp === len.U)

  // Opcodes (7-bit LSBs)
  val PUSH = "b0100111".U(7.W)
  val POP  = "b1000011".U(7.W)
  val PEEK = "b1000000".U(7.W)

  when (opcode === PUSH){
    when (io.isFull){
      overflowReg := true.B
      outReg := 0.U
    }
    .otherwise{
      stack(sp) := payload(dataWidth -1, 0)
      sp := sp + 1.U
    }
  }
  .elsewhen (opcode === POP){
    when (io.isEmpty){
      underflowReg := true.B
      outReg := 0.U
    }
    .otherwise{
      outReg := stack(sp - 1.U)
      sp := sp - 1.U
      poppedReg := true.B
    }
  }
  .elsewhen (opcode === PEEK){
    when(io.isEmpty){
      underflowReg := true.B
      outReg := 0.U
    }
    .otherwise{
      outReg := stack(sp - 1.U)
      peekedReg := true.B
    }
  }

  io.overflow := overflowReg
  io.underflow := underflowReg
  io.peeked := peekedReg
  io.popped := poppedReg
}
// Your code ends here

object SVGen extends App {
  val out = Paths.get(
    "out",
    this.getClass
      .getName
      .stripSuffix("$")
  ).toString
  new ChiselStage().emitSystemVerilog(
    new StackModule(args(0).toInt, args(1).toInt),
    Array("--target-dir", out),
  )
}