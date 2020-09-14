using System;
using GbSharp.Core.Exceptions;

namespace GbSharp.Core.CPU
{
    public class CPU
    {
        private Registers _registers;
        private MemoryBus _bus;
        private ushort _pc;
        private ushort _sp;
        private ushort _t;

        private bool _interrupts = true;
        private bool _halted;
        private bool _stopped;

        private void Step()
        {
            if (_stopped) throw new CpuStoppedException();

            var instructionByte = _bus.Read(_pc);
            var prefixed = instructionByte == 0xCB;

            if (prefixed) instructionByte = _bus.Read(IncPC(1));

            var instruction = FromByte(instructionByte, prefixed);
            var nextPc = Execute(instruction);

            _pc = nextPc;
        }

        private ushort Execute(Instruction instruction) => _halted ? _pc : instruction.Execute();

        private Instruction FromByte(byte instructionByte, bool prefixed) => prefixed ? FromBytePrefixed(instructionByte) : FromByteNotPrefixed(instructionByte);

        // 0xCBnn
        private Instruction FromBytePrefixed(byte instructionByte) => instructionByte switch
        {
            _ => throw new UnknownInstructionException($"Unknown instruction found for: 0xCB{instructionByte:X}")
        };

        // 0xnn
        private Instruction FromByteNotPrefixed(byte instructionByte) => instructionByte switch
        {
            #region Misc controls

            0x00 => new Instruction("NOP", Noop),
            0x10 => new Instruction("STOP 0", Stop),
            0x76 => new Instruction("HALT", Halt),
            0xF3 => new Instruction("DI", DisableInterrupts),
            0xFB => new Instruction("EI", EnableInterrupts),

            #endregion

            #region Jumps/calls

            0x18 => new Instruction("JR r8", () => RelativeJump()),
            0x20 => new Instruction("JR NZ,r8", () => RelativeJump(!_registers.F.Zero)),
            0x28 => new Instruction("JR Z,r8", () => RelativeJump(_registers.F.Zero)),
            0x30 => new Instruction("JR NC,r8", () => RelativeJump(!_registers.F.Carry)),
            0x38 => new Instruction("JR C,r8", () => RelativeJump(_registers.F.Carry)),
            
            0xC2 => new Instruction("JP NZ,a16", () => Jump(!_registers.F.Zero)),
            0xC3 => new Instruction("JP a16", () => Jump()),
            0xCA => new Instruction("JP Z,a16", () => Jump(_registers.F.Zero)),
            0xD2 => new Instruction("JP NC,a16", () => Jump(!_registers.F.Carry)),
            0xDA => new Instruction("JP C,a16", () => Jump(_registers.F.Carry)),
            0xE9 => new Instruction("JP (HL)", JumpHL),

            0xC0 => new Instruction("RET NZ", () => Return(!_registers.F.Zero)),
            0xC8 => new Instruction("RET Z", () => Return(_registers.F.Zero)),
            0xC9 => new Instruction("RET", Return),
            0xD0 => new Instruction("RET NC", () => Return(!_registers.F.Carry)),
            0xD8 => new Instruction("RET C", () => Return(_registers.F.Carry)),
            0xD9 => new Instruction("RETI", ReturnInterrupts),

            0xC4 => new Instruction("CALL NZ,a16", () => Call(!_registers.F.Zero)),
            0xCC => new Instruction("CALL Z,a16", () => Call(_registers.F.Zero)),
            0xCD => new Instruction("CALL a16", () => Call()),
            0xD4 => new Instruction("CALL NC,a16", () => Call(!_registers.F.Carry)),
            0xDC => new Instruction("CALL C,a16", () => Call(_registers.F.Carry)),

            0xC7 => new Instruction("RST 00H", () => CallRoutine(0x0)),
            0xCF => new Instruction("RST 08H", () => CallRoutine(0x8)),
            0xD7 => new Instruction("RST 10H", () => CallRoutine(0x10)),
            0xDF => new Instruction("RST 18H", () => CallRoutine(0x18)),
            0xE7 => new Instruction("RST 20H", () => CallRoutine(0x20)),
            0xEF => new Instruction("RST 28H", () => CallRoutine(0x28)),
            0xF7 => new Instruction("RST 30H", () => CallRoutine(0x30)),
            0xFF => new Instruction("RST 38H", () => CallRoutine(0x38)),

            #endregion

            #region 8bit load/store/move

            0x02 => new Instruction("LD (BC),A", () => LoadToAddress(_registers.BC, _registers.A)),
            0x06 => new Instruction("LD B,d8", () => LoadFromByte(out _registers.B)),
            0x0A => new Instruction("LD A,(BC)", () => LoadFromAddress(out _registers.A, _registers.BC)),
            0x0E => new Instruction("LD C,d8", () => LoadFromByte(out _registers.C)),
            0x12 => new Instruction("LD (DE),A", () => LoadToAddress(_registers.DE, _registers.A)),
            0x16 => new Instruction("LD D,d8", () => LoadFromByte(out _registers.D)),
            0x1A => new Instruction("LD A,(DE)", () => LoadFromAddress(out _registers.A, _registers.DE)),
            0x1E => new Instruction("LD E,d8", () => LoadFromByte(out _registers.E)),
            0x22 => new Instruction("LD (HL+),A", () => LoadToHLI(_registers.A)),
            0x26 => new Instruction("LD H,d8", () => LoadFromByte(out _registers.H)),
            0x2A => new Instruction("LD A,(HL+)", () => LoadFromHLI(out _registers.A)),
            0x2E => new Instruction("LD L,d8", () => LoadFromByte(out _registers.L)),
            0x32 => new Instruction("LD (HL-),A", () => LoadToHLD(_registers.A)),
            0x36 => new Instruction("LD (HL),d8", () => LoadByteToAddress(_registers.HL)),
            0x3A => new Instruction("LD A,(HL-)", () => LoadFromHLD(out _registers.A)),
            0x3E => new Instruction("LD A,d8", () => LoadFromByte(out _registers.A)),

            0x40 => new Instruction("LD B,B", () => LoadToRegister(out _registers.B, _registers.B)),
            0x41 => new Instruction("LD B,C", () => LoadToRegister(out _registers.B, _registers.C)),
            0x42 => new Instruction("LD B,D", () => LoadToRegister(out _registers.B, _registers.D)),
            0x43 => new Instruction("LD B,E", () => LoadToRegister(out _registers.B, _registers.E)),
            0x44 => new Instruction("LD B,H", () => LoadToRegister(out _registers.B, _registers.H)),
            0x45 => new Instruction("LD B,L", () => LoadToRegister(out _registers.B, _registers.L)),
            0x46 => new Instruction("LD B,(HL)", () => LoadFromAddress(out _registers.B, _registers.HL)),
            0x47 => new Instruction("LD B,A", () => LoadToRegister(out _registers.B, _registers.A)),

            0x48 => new Instruction("LD C,B", () => LoadToRegister(out _registers.C, _registers.B)),
            0x49 => new Instruction("LD C,C", () => LoadToRegister(out _registers.C, _registers.C)),
            0x4A => new Instruction("LD C,D", () => LoadToRegister(out _registers.C, _registers.D)),
            0x4B => new Instruction("LD C,E", () => LoadToRegister(out _registers.C, _registers.E)),
            0x4C => new Instruction("LD C,H", () => LoadToRegister(out _registers.C, _registers.H)),
            0x4D => new Instruction("LD C,L", () => LoadToRegister(out _registers.C, _registers.L)),
            0x4E => new Instruction("LD C,(HL)", () => LoadFromAddress(out _registers.C, _registers.HL)),
            0x4F => new Instruction("LD C,A", () => LoadToRegister(out _registers.C, _registers.A)),

            0x50 => new Instruction("LD D,B", () => LoadToRegister(out _registers.D, _registers.B)),
            0x51 => new Instruction("LD D,C", () => LoadToRegister(out _registers.D, _registers.C)),
            0x52 => new Instruction("LD D,D", () => LoadToRegister(out _registers.D, _registers.D)),
            0x53 => new Instruction("LD D,E", () => LoadToRegister(out _registers.D, _registers.E)),
            0x54 => new Instruction("LD D,H", () => LoadToRegister(out _registers.D, _registers.H)),
            0x55 => new Instruction("LD D,L", () => LoadToRegister(out _registers.D, _registers.L)),
            0x56 => new Instruction("LD D,(HL)", () => LoadFromAddress(out _registers.D, _registers.HL)),
            0x57 => new Instruction("LD D,A", () => LoadToRegister(out _registers.D, _registers.A)),

            0x58 => new Instruction("LD E,B", () => LoadToRegister(out _registers.E, _registers.B)),
            0x59 => new Instruction("LD E,C", () => LoadToRegister(out _registers.E, _registers.C)),
            0x5A => new Instruction("LD E,D", () => LoadToRegister(out _registers.E, _registers.D)),
            0x5B => new Instruction("LD E,E", () => LoadToRegister(out _registers.E, _registers.E)),
            0x5C => new Instruction("LD E,H", () => LoadToRegister(out _registers.E, _registers.H)),
            0x5D => new Instruction("LD E,L", () => LoadToRegister(out _registers.E, _registers.L)),
            0x5E => new Instruction("LD E,(HL)", () => LoadFromAddress(out _registers.E, _registers.HL)),
            0x5F => new Instruction("LD E,A", () => LoadToRegister(out _registers.E, _registers.A)),

            0x60 => new Instruction("LD H,B", () => LoadToRegister(out _registers.H, _registers.B)),
            0x61 => new Instruction("LD H,C", () => LoadToRegister(out _registers.H, _registers.C)),
            0x62 => new Instruction("LD H,D", () => LoadToRegister(out _registers.H, _registers.D)),
            0x63 => new Instruction("LD H,E", () => LoadToRegister(out _registers.H, _registers.E)),
            0x64 => new Instruction("LD H,H", () => LoadToRegister(out _registers.H, _registers.H)),
            0x65 => new Instruction("LD H,L", () => LoadToRegister(out _registers.H, _registers.L)),
            0x66 => new Instruction("LD H,(HL)", () => LoadFromAddress(out _registers.H, _registers.HL)),
            0x67 => new Instruction("LD H,A", () => LoadToRegister(out _registers.H, _registers.A)),

            0x68 => new Instruction("LD L,B", () => LoadToRegister(out _registers.L, _registers.B)),
            0x69 => new Instruction("LD L,C", () => LoadToRegister(out _registers.L, _registers.C)),
            0x6A => new Instruction("LD L,D", () => LoadToRegister(out _registers.L, _registers.D)),
            0x6B => new Instruction("LD L,E", () => LoadToRegister(out _registers.L, _registers.E)),
            0x6C => new Instruction("LD L,H", () => LoadToRegister(out _registers.L, _registers.H)),
            0x6D => new Instruction("LD L,L", () => LoadToRegister(out _registers.L, _registers.L)),
            0x6E => new Instruction("LD L,(HL)", () => LoadFromAddress(out _registers.L, _registers.HL)),
            0x6F => new Instruction("LD L,A", () => LoadToRegister(out _registers.L, _registers.A)),

            0x70 => new Instruction("LD (HL),B", () => LoadToAddress(_registers.HL, _registers.B)),
            0x71 => new Instruction("LD (HL),C", () => LoadToAddress(_registers.HL, _registers.C)),
            0x72 => new Instruction("LD (HL),D", () => LoadToAddress(_registers.HL, _registers.D)),
            0x73 => new Instruction("LD (HL),E", () => LoadToAddress(_registers.HL, _registers.E)),
            0x74 => new Instruction("LD (HL),H", () => LoadToAddress(_registers.HL, _registers.H)),
            0x75 => new Instruction("LD (HL),L", () => LoadToAddress(_registers.HL, _registers.L)),
            0x77 => new Instruction("LD (HL),A", () => LoadToAddress(_registers.HL, _registers.A)),

            0x78 => new Instruction("LD A,B", () => LoadToRegister(out _registers.A, _registers.B)),
            0x79 => new Instruction("LD A,C", () => LoadToRegister(out _registers.A, _registers.C)),
            0x7A => new Instruction("LD A,D", () => LoadToRegister(out _registers.A, _registers.D)),
            0x7B => new Instruction("LD A,E", () => LoadToRegister(out _registers.A, _registers.E)),
            0x7C => new Instruction("LD A,H", () => LoadToRegister(out _registers.A, _registers.H)),
            0x7D => new Instruction("LD A,L", () => LoadToRegister(out _registers.A, _registers.L)),
            0x7E => new Instruction("LD A,(HL)", () => LoadFromAddress(out _registers.A, _registers.HL)),
            0x7F => new Instruction("LD A,A", () => LoadToRegister(out _registers.A, _registers.A)),

            0xE0 => new Instruction("LDH (a8),A", LoadToByteAddress),
            0xE2 => new Instruction("LD (C),A", LoadToCAddress),
            0xEA => new Instruction("LD (a16),A", LoadToWordAddress),
            0xF0 => new Instruction("LDH A,(a8)", LoadFromByteAddress),
            0xF2 => new Instruction("LDH A,(C)", LoadFromCAddress),
            0xFA => new Instruction("LDH A,(a16)", LoadFromWordAddress),

            #endregion

            #region 16bit load/store/move

            0x01 => new Instruction("LD BC,d16", () => LoadFromWord(value => _registers.BC = value)),
            0x11 => new Instruction("LD DE,d16", () => LoadFromWord(value => _registers.DE = value)),
            0x21 => new Instruction("LD HL,d16", () => LoadFromWord(value => _registers.HL = value)),
            0x31 => new Instruction("LD SP,d16", () => LoadFromWord(out _sp)),

            0xC1 => new Instruction("POP BC", () => PopToRegister(value => _registers.BC = value)),
            0xC5 => new Instruction("PUSH BC", () => PushFromRegister(_registers.BC)),
            0xD1 => new Instruction("POP DE", () => PopToRegister(value => _registers.DE = value)),
            0xD5 => new Instruction("PUSH DE", () => PushFromRegister(_registers.DE)),
            0xE1 => new Instruction("POP HL", () => PopToRegister(value => _registers.HL = value)),
            0xE5 => new Instruction("PUSH HL", () => PushFromRegister(_registers.HL)),
            0xF1 => new Instruction("POP AF", () => PopToRegister(value => _registers.AF = value)),
            0xF5 => new Instruction("PUSH AF", () => PushFromRegister(_registers.AF)),
            
            0x08 => new Instruction("LD (a16),SP", LoadFromSPToAddress),
            0xF8 => new Instruction("LD HL,SP+r8", LoadFromSPnToHL),
            0xF9 => new Instruction("LD SP,HL", LoadFromHLToSP),

            #endregion

            #region 8bit arithmetic/logical instructions

            0x04 => new Instruction("INC B", () => Increment(ref _registers.B)),
            0x0C => new Instruction("INC C", () => Increment(ref _registers.C)),
            0x14 => new Instruction("INC D", () => Increment(ref _registers.D)),
            0x1C => new Instruction("INC E", () => Increment(ref _registers.E)),
            0x24 => new Instruction("INC H", () => Increment(ref _registers.H)),
            0x2C => new Instruction("INC L", () => Increment(ref _registers.L)),
            0x34 => new Instruction("INC (HL)", () => IncrementAddress(_registers.HL)),
            0x3C => new Instruction("INC A", () => Increment(ref _registers.A)),

            0x05 => new Instruction("DEC B", () => 0),
            0x0D => new Instruction("DEC C", () => 0),
            0x15 => new Instruction("DEC D", () => 0),
            0x1D => new Instruction("DEC E", () => 0),
            0x25 => new Instruction("DEC H", () => 0),
            0x2D => new Instruction("DEC L", () => 0),
            0x35 => new Instruction("DEC (HL)", () => 0),
            0x3D => new Instruction("DEC A", () => 0),
            
            0x27 => new Instruction("DAA", () => 0),
            0x37 => new Instruction("SCF", () => 0),

            #endregion

            // Unknown instruction
            _ => throw new UnknownInstructionException($"Unknown instruction found for: 0x{instructionByte:X}")
        };

        #region Misc controls

        private ushort Noop()
        {
            _t = 4;
            return IncPC(1);
        }

        private ushort Stop()
        {
            _stopped = true;
            _t = 4;
            return IncPC(2);
        }

        private ushort Halt()
        {
            _halted = true;
            _t = 4;
            return IncPC(1);
        }

        private ushort EnableInterrupts()
        {
            _interrupts = true;
            _t = 4;
            return IncPC(1);
        }

        private ushort DisableInterrupts()
        {
            _interrupts = false;
            _t = 4;
            return IncPC(1);
        }

        #endregion

        #region Jumps/calls

        private ushort Jump(bool shouldJump = true)
        {
            if (shouldJump)
            {
                _t = 16;
                return _bus.ReadWord(_pc + 1);
            }

            _t = 12;
            return IncPC(3);
        }

        private ushort JumpHL()
        {
            _t = 4;
            return _registers.HL;
        }

        private ushort RelativeJump(bool shouldJump = true)
        {
            if (shouldJump)
            {
                var d8 = _bus.Read(_pc + 1);
                var s8 = (d8 & 127) - (d8 & 128);

                _t = 12;
                return (ushort)(_pc + s8);
            }

            _t = 8;
            return IncPC(2);
        }

        private ushort Call(bool shouldJump = true)
        {
            var nextPc = IncPC(3);

            if (shouldJump)
            {
                Push(nextPc);
                _t = 24;
                return _bus.ReadWord(_pc + 1);
            }

            _t = 12;
            return nextPc;
        }

        private ushort CallRoutine(ushort address)
        {
            Push(IncPC(1));
            _t = 16;
            return address;
        }

        private ushort Return()
        {
            _t = 16;
            return Pop();
        }

        private ushort ReturnInterrupts()
        {
            _interrupts = true;
            _t = 16;
            return Pop();
        }

        private ushort Return(bool shouldJump)
        {
            if (shouldJump)
            {
                _t = 20;
                return Pop();
            }

            _t = 8;
            return IncPC(1);
        }

        #endregion

        #region 8bit load/store/move

        private ushort LoadToRegister(out byte register, byte value)
        {
            register = value;
            _t = 4;
            return IncPC(1);
        }

        private ushort LoadFromAddress(out byte register, ushort address)
        {
            register = _bus.Read(address);
            _t = 8;
            return IncPC(1);
        }

        private ushort LoadFromByte(out byte register)
        {
            register = _bus.Read(_pc + 1);
            _t = 8;
            return IncPC(2);
        }

        private ushort LoadFromHLI(out byte register)
        {
            register = _bus.Read(_registers.HL);
            _registers.HL++;
            _t = 8;
            return IncPC(1);
        }

        private ushort LoadFromHLD(out byte register)
        {
            register = _bus.Read(_registers.HL);
            _registers.HL--;
            _t = 8;
            return IncPC(1);
        }

        private ushort LoadToAddress(ushort address, byte value)
        {
            _bus.Write(address, value);
            _t = 8;
            return IncPC(1);
        }

        private ushort LoadByteToAddress(ushort address)
        {
            _bus.Write(address, _bus.Read(_pc + 1));
            _t = 12;
            return IncPC(2);
        }

        private ushort LoadToHLI(byte value)
        {
            _bus.Write(_registers.HL, value);
            _registers.HL++;
            _t = 8;
            return IncPC(1);
        }

        private ushort LoadToHLD(byte value)
        {
            _bus.Write(_registers.HL, value);
            _registers.HL--;
            _t = 8;
            return IncPC(1);
        }

        private ushort LoadToByteAddress()
        {
            _bus.Write(0xFF00 | _bus.Read(_pc + 1), _registers.A);
            _t = 12;
            return IncPC(2);
        }

        private ushort LoadToCAddress()
        {
            _bus.Write(0xFF00 | _registers.C, _registers.A);
            _t = 8;
            return IncPC(2);
        }

        private ushort LoadToWordAddress()
        {
            _bus.Write(_bus.ReadWord(_pc + 1), _registers.A);
            _t = 16;
            return IncPC(3);
        }

        private ushort LoadFromByteAddress()
        {
            _registers.A = _bus.Read(0xFF00 | _bus.Read(_pc + 1));
            _t = 12;
            return IncPC(2);
        }

        private ushort LoadFromCAddress()
        {
            _registers.A = _bus.Read(0xFF00 | _registers.C);
            _t = 8;
            return IncPC(2);
        }

        private ushort LoadFromWordAddress()
        {
            _registers.A = _bus.Read(_bus.ReadWord(_pc + 1));
            _t = 16;
            return IncPC(3);
        }

        #endregion

        #region 16bit load/store/move

        private ushort LoadFromWord(Action<ushort> setter)
        {
            setter(_bus.ReadWord(_pc + 1));
            _t = 12;
            return IncPC(3);
        }

        private ushort LoadFromWord(out ushort destination)
        {
            destination = _bus.ReadWord(_pc + 1);
            _t = 12;
            return IncPC(3);
        }

        private ushort LoadFromSPToAddress()
        {
            _bus.WriteWord(_bus.ReadWord(_pc + 1), _sp);
            _t = 20;
            return IncPC(3);
        }

        private ushort LoadFromSPnToHL()
        {
            var d8 = _bus.Read(_pc + 1);
            var s8 = (d8 & 127) - (d8 & 128);
            var result = unchecked(_sp + s8);
            var newValue = (ushort)(result & 0xFFFF);

            if (s8 >= 0)
            {
                _registers.F.Carry = (newValue & 0xFF) + s8 > 0xFF;
                _registers.F.HalfCarry = (s8 & 0xF) + (_sp & 0xF) > 0xF;
            }
            else
            {
                _registers.F.Carry = (newValue & 0xFF) <= (_sp & 0xFF);
                _registers.F.HalfCarry = (newValue & 0xF) <= (_sp & 0xF);
            }

            _registers.F.Zero = false;
            _registers.F.Subtract = false;

            _registers.HL = newValue;

            _t = 12;
            return IncPC(2);
        }

        private ushort LoadFromHLToSP()
        {
            _registers.HL = _sp;
            _t = 8;
            return IncPC(1);
        }

        private ushort PopToRegister(Action<ushort> setter)
        {
            setter(Pop());
            _t = 12;
            return IncPC(1);
        }

        private ushort PushFromRegister(ushort value)
        {
            Push(value);
            _t = 16;
            return IncPC(1);
        }

        #endregion

        #region 8bit arithmetic/logical instructions

        private ushort Increment(ref byte register)
        {
            _registers.F.Subtract = false;
            _registers.F.HalfCarry = register == 0xF;

            register++;

            _registers.F.Zero = register == 0b0;

            _t = 4;
            return IncPC(1);
        }

        private ushort IncrementAddress(ushort address)
        {
            var value = _bus.Read(address);
            var newValue = value++;

            _registers.F.Subtract = false;
            _registers.F.HalfCarry = value == 0xF;
            _registers.F.Zero = newValue == 0b0;

            _bus.Write(address, newValue);

            _t = 12;
            return IncPC(12);
        }

        #endregion

        #region Instructions implementations

        private byte Add(byte value)
        {
            var result = unchecked(_registers.A + value);
            var newValue = (byte)(result & 0xFF);

            _registers.F.Zero = newValue == 0;
            _registers.F.Subtract = false;
            _registers.F.Carry = result != newValue;
            _registers.F.HalfCarry = (_registers.A & 0xF) + (value & 0xF) > 0xF;

            return newValue;
        }

        #endregion

        #region Pointers

        private ushort IncPC(byte value)
        {
            #if DEBUG
            return checked((ushort)(_pc + value));
            #else
            return (ushort)(_pc + value);
            #endif
        }

        private ushort DecSP(byte value)
        {
            #if DEBUG
            return checked((ushort)(_sp + value));
            #else
            return (ushort)(_sp + value);
            #endif
        }

        private ushort IncSP(byte value)
        {
            #if DEBUG
            return checked((ushort)(_sp - value));
            #else
            return (ushort)(_sp - value);
            #endif
        }

        private void Push(ushort value)
        {
            _sp = IncSP(1);
            _bus.Write(_sp, (byte)((value & 0xFF00) >> 8));

            _sp = IncSP(1);
            _bus.Write(_sp, (byte)(value & 0xFF));
        }

        private ushort Pop()
        {
            var leastByte = (ushort)_bus.Read(_sp);
            _sp = DecSP(1);

            var mostByte = (ushort) _bus.Read(_sp);
            _sp = DecSP(1);

            return (ushort)((mostByte << 8) | leastByte);
        }

        #endregion
    }
}
