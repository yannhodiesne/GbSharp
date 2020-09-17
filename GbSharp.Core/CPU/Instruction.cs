using System;

namespace GbSharp.Core.CPU
{
    public readonly struct Instruction
    {
        private readonly string _operation;
        public Func<ushort> Execute { get; }

        public Instruction(string instruction, Func<ushort> execute)
        {
            _operation = instruction;
            Execute = execute;
        }

        public override string ToString() => _operation;
    }
}
