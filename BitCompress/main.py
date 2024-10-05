import struct


def get_bits(n):
    """
  Restituisce una lista con i bit di un intero dato in input.

  Args:
    n: L'intero di input.

  Returns:
    Una lista con i bit dell'intero, dal bit meno significativo al bit più significativo.
  """
    bits = []
    while n > 0:
        bits.append(n % 2)
        n //= 2
    return bits  # Inverte la lista per avere l'ordine corretto


def float_to_bits(f, size=32):
    """Converte un numero float in una stringa di bit.

  Args:
    f: Il numero float da convertire.
    size: La dimensione del float in bit (32 o 64). Default è 32.

  Returns:
    Una stringa che rappresenta il numero float in binario.
  """

    if size == 32:
        fmt = 'f'
    elif size == 64:
        fmt = 'd'
    else:
        raise ValueError("Size deve essere 32 o 64")

    # Impacchetta il float come stringa di byte
    byte_string = struct.pack(fmt, f)

    # Converti la stringa di byte in una sequenza di bit
    bits = ''.join(f'{byte:08b}' for byte in byte_string)
    return bits


# print(float_to_bits(10.5, size=64))

#########################################################
#########################################################
#########################################################

# Generic functions
def gates_pins_to_indexes(pins):
    indexes = []
    for pin in pins:
        if pin.is_base_pin:
            indexes.append(pin.get_port())
    return indexes

def get_not_pins(pins):
    nots = []
    for pin in pins:
        if pin.gate == 'not' and pin.is_base_pin:
            nots.append(pin.args[0])
    return nots

#########################################################

gates = ['pin', 'not', 'and', 'or']
#enumerateGates = enumerate(gates)

debugHash = True
hideUnusedImplicitPins = True
addImplicitNotPortOnBrothers = False

class GateBranch:
    def __init__(self, map, gate, value=-1, is_basic = True):
        self.map: BitsMap = map

        self.gate = gate
        self.i_gate = gates.index(gate)
        self.value = value
        self.args: [GateBranch] = []

        # Optimization
        self.usage = 0
        self.children: [GateBranch] = []
        self.down_complexity = -1
        self.up_complexity = -1
        self.is_base_pin = True # if contains only elementary pins (or not pins)
        self.is_basic = is_basic # is basic table definition gate
        self.max_port = 0 if gate != 'pin' else value
        self.implicit = 1 if self.is_basic and self.gate == 'and' else 0 # 0 => no implicit, 1 => normal implicit (or not implicit if or)
        self.implicit_not = False #todo: Implement it (?)

        # 50 shades of ports
        self.ports = [] if gate != 'pin' else [value]
        self.involved_ports = {}
        self.num_involved_ports = -1

        if gate == 'pin':
            self.max_port = value
            self.involved_ports[value] = 1

        # Cache
        self.last_hash = None

    def calculate_args_in_ports(self, args_in_ports=None):
        if args_in_ports is None:
            args_in_ports = {}

        for arg in self.args:
            if self.gate == 'or' or not self.is_basic:
                arg.calculate_args_in_ports(args_in_ports)
            else: # is 'and'
                argPorts = arg if arg.gate != 'not' else arg.args[0]
                for port in argPorts.ports:
                    if port not in args_in_ports:
                        args_in_ports[port] = []
                    args_in_ports[port].append(arg)

        return args_in_ports

    def calc_complexity(self, complexity=0):
        # Calculate num involved ports
        self.num_involved_ports = 0
        for port, val in self.involved_ports.items():
            if val > 0:
                self.num_involved_ports += 1

        # Calculate complexity
        self.down_complexity = complexity

        complexity += 1
        max_complexity = 0
        for child in self.children:
            compl = child.calc_complexity(complexity)
            if compl > max_complexity:
                max_complexity = compl

        self.up_complexity = max_complexity
        return self.up_complexity + 1

    def increment_usage(self):
        self.usage += 1

    def get_base_pins(self):
        #todo: cache into self.ports
        if self.is_base_pin:
            return [self.get_port()]

        if self.gate == 'not':
            return self.args[0].get_base_pins()

        base_pins = []
        for arg in self.args:
            if arg.is_base_pin:
                base_pins.append(arg)

        return base_pins

    def get_base_pin(self, index):
        for arg in self.args:
            if arg.is_base_pin and arg.get_port() == index:
                return arg

        return None

    def get_port(self):
        if not self.is_base_pin or self.i_gate > 1:
            return -1

        if self.gate == 'not':
            return self.args[0].value

        return self.value

    def get_port_not(self, port):
        if not self.is_base_pin:
            return -1

        for arg in self.args:
            if arg.get_port() == port:
                return 1 if arg.gate == 'not' else 0

        return -1

    def add_implicit_port(self, index, update=True):
        notGate = GateBranch(self, 'not')
        notGate.add(self.map.pins[index])
        notGate = self.map.check_gate(notGate)
        self.add(notGate)

        if update:
            self.propagate_update()

        return notGate

    def add_implicit_port_upTo(self, upToIndex):
        addedPorts = []

        for i in range(self.max_port, upToIndex):
            notGate = self.add_implicit_port(i+1, False)
            addedPorts.append(notGate)

        self.propagate_update()

        return addedPorts

    def add_involved_ports(self, ports):
        for port, num in ports.items():
            if port not in self.involved_ports:
                self.involved_ports[port] = 0
                if num > 0:
                    self.ports.append(port)

            self.involved_ports[port] += num

        self.ports.sort()

    def remove_involved_ports(self, ports):
        for port, num in ports.items():
            self.involved_ports[port] -= num

            if self.involved_ports[port] == 0:
                self.ports.remove(port)

    def set_always_true(self):
        print("OR always true")
        pass #todo: ok, this gate is always true

    def optimize_or(self):
        if self.gate != 'or':
            return

        if False: # this implementation has no more sense with 0 bits ignoring
            # First of all, check opposite gates (and duplicates)
            hashes_one = {}
            hashes_zero = {}
            to_remove = []

            for arg in self.args:
                hashes = hashes_one
                opposite_hashes = hashes_zero

                if arg.gate == 'not':
                    hashes = hashes_zero
                    opposite_hashes = hashes_one
                    hash = arg.args[0].get_hash()
                else:
                    hash = arg.get_hash()

                if hash in hashes:
                    to_remove.append(arg)
                else:
                    if hash in opposite_hashes:
                        # OR of opposite pins is always true
                        self.set_always_true()
                        return
                    else:
                        hashes[hash] = arg

            for rem in to_remove:
                self.args.remove(rem)

            # Convert NOT AND to OR NOTS
            # !(A*B) => !A + !B + NOT_IMPLICIT
            if self.implicit == 0:
                for arg in self.args:
                    if arg.gate == 'not' and not arg.is_base_pin:
                        arg_and = arg.args[0]

                        if arg_and.gate != 'and':
                            print("This could be weird (65476467)")
                            continue

                        arg_base_pins = arg.get_base_pins()

                        for base_pin in arg_base_pins:
                            not_base_pin = GateBranch(self.map, 'not')
                            not_base_pin.add(base_pin)
                            not_base_pin = self.map.check_gate(not_base_pin)
                            self.add(not_base_pin, in_process=True)

                        self.implicit = 1
                        self.remove(arg)
                        break

            if self.implicit == 1:
                base_pins = self.get_base_pins()
                base_pins_not = get_not_pins(base_pins)
                for arg in self.args:
                    if arg.gate == 'not' and not arg.is_base_pin: # and arg.is_basic
                        arg_and = arg.args[0]

                        if arg_and.gate != 'and':
                            print("This could be weird (3294538)")
                            continue

                        arg_base_pins = arg_and.get_base_pins()
                        for arg_base_pin in arg_base_pins:
                            if arg_base_pin not in base_pins_not:
                                self.set_always_true()
                                return

                        for base_pin in base_pins_not:
                            if base_pin not in arg_base_pins:
                                self.set_always_true()
                                return

        # Notes:
        # (A*B)+(A*!B) => [basic_state] (A*B)+A => A
        args_in_ports = self.calculate_args_in_ports()


    def remove(self, arg):
        if arg in self.args:
            self.args.remove(arg)

            if self in arg.children:
                arg.children.remove(self)

            self.remove_involved_ports(arg.involved_ports)
            arg.check_if_used()

    def add(self, arg, at=-1, in_process=False):

        if arg.is_base_pin:
            if arg.max_port > self.max_port:
                self.max_port = arg.max_port

        self.add_involved_ports(arg.involved_ports)

        if self.i_gate > 1 or (self.gate == 'not' and not arg.is_base_pin):
            self.is_base_pin = False

        arg.children.append(self)

        if at < 0:
            self.args.append(arg)
        else:
            self.args.insert(at, arg)

        if not in_process: # remove redundancies in OR
            self.optimize_or()

        self.propagate_update()

    def destroy(self, replace_with=None):
        for child in self.children:
            if replace_with is not None:
                if child not in replace_with.children:
                    pos = child.args.index(self)
                    child.add(replace_with, at=pos)

            child.args.remove(self)

        self.check_if_used()

    def check_if_used(self):
        if len(self.children) == 0:
            hash = self.get_hash()
            if hash in self.map.gates:
                del self.map.gates[hash]

    def propagate_update(self):
        prevHash = None
        if self.last_hash is not None and self.last_hash in self.map.gates:
            prevHash = self.last_hash

        self.last_hash = None
        _hash = self.get_hash()

        if prevHash is not None:
            del self.map.gates[prevHash]
            self.map.gates[_hash] = self

        for child in self.children:
            child.propagate_update()

        return _hash

    def __repr__(self):
        return self.get_hash()

    def get_hash(self) -> str:
        if self.last_hash is not None:
            return self.last_hash

        hash = ''

        if self.implicit:
            if self.implicit_not:
                hash += '!'
            else:
                hash += '&'

        hash += str(self.i_gate)

        if self.value >= 0:
            hash += ':' + str(self.value)

        if len(self.args) > 0:
            if debugHash:
                hash += "("

            first = debugHash
            for arg in self.args:
                if first:
                    first = False
                else:
                    hash += ','

                hash += arg.get_hash()

            if debugHash:
                hash += ")"

        self.last_hash = hash
        return hash


# Implement XOR only on final circuit (?)
# Remember: XOR = (A AND (NOT B)) OR ((NOT A) AND B)
# XOR(A, B, C) = (A & ~B & ~C) | (~A & B & ~C) | (~A & ~B & C) | (A & B & C) => XOR(XOR(A, B),C)

disableCheckGateOptimize = False
ignoreNotPin = True
ignoreNotBit = True

class BitsMap:
    def __init__(self):
        self.pins: [GateBranch] = []
        self.gates = {}
        self.map = GateBranch(self, 'or')

    def final_compression(self):
        for pin in self.pins:
            pin.calc_complexity()

    def check_gate(self, gate) -> GateBranch:
        hash = gate.get_hash()
        if hash in self.gates:
            return self.gates[hash]
        else:
            self.gates[hash] = gate
            #gate.optimize() if not disableCheckGateOptimize else None
            return gate

    def set(self, series, bit):
        if ignoreNotBit and bit == 0:
            return

        andGate = GateBranch(self, 'and')

        for i in range(0, len(series)):
            if len(self.pins) <= i:
                pin = GateBranch(self, 'pin', i)
                self.pins.append(pin)

            pin = self.pins[i]
            pin.increment_usage()

            gate = pin

            if series[i] == 0: # != bit
                if ignoreNotPin:
                    continue

                notGate = GateBranch(self, 'not')
                notGate.add(pin)
                gate = self.check_gate(notGate)
                gate.increment_usage()

            andGate.add(gate)

        andGate.implicit = True
        andGate.implicit_not = True
        andGate = self.check_gate(andGate)
        andGate.increment_usage()

        self.map.add(andGate)

map = BitsMap()

if False:
    map.set([0], 1)
    map.set([1], 1)
    map.set([0, 1], 1)
    map.set([1, 1], 0)

if True:
    map.set([0], 1)
    map.set([1],0)
    map.set([0, 1], 0)
    map.set([1, 1], 1)

if False: # unneeded pin example
    map.set([0], 1)
    map.set([1], 0)
    map.set([0, 1], 1)
    map.set([1, 1], 0)

map.final_compression()

print("check")

# Esempio di utilizzo
'''
numero = 2
bit_list = get_bits(numero)
print(f"I bit di {numero} sono: {bit_list}")
'''
