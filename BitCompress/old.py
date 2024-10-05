# Snippet #1
# AND opposites management
if False:  # this implementation has no more sense if ignoring 0 bits
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
            if arg.gate == 'not' and not arg.is_base_pin:  # and arg.is_basic
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