using System;

namespace GbSharp.Core.Exceptions
{
    public class UnknownInstructionException : Exception
    {
        public UnknownInstructionException(string message) : base(message)
        {

        }
    }
}
