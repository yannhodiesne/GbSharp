namespace GbSharp.Core.CPU
{
    readonly struct MemoryBus
    {
        private readonly byte[] _memory;

        public MemoryBus(byte[] data)
        {
            _memory = new byte[0xFFFF];
            data.CopyTo(_memory, 0);
        }

        internal byte Read(ushort address) => _memory[address];

        internal byte Read(int address) => _memory[(ushort) address];

        internal ushort ReadWord(ushort address) => (ushort)((_memory[address + 1] << 8) | _memory[address]);

        internal ushort ReadWord(int address) => ReadWord((ushort) address);

        internal void Write(ushort address, byte value) => _memory[address] = value;

        internal void Write(int address, byte value) => _memory[(ushort) address] = value;

        internal void WriteWord(ushort address, ushort value)
        {
            _memory[address] = (byte) (value & 0xFF);
            _memory[address + 1] = (byte) ((value & 0xFF00) >> 8);
        }

        // TODO: maybe reverse ReadWord and WriteWord byte order ? (Little vs Big Endian)
    }
}
