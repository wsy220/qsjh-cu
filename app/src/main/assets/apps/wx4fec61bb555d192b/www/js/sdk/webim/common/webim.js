(function(global, factory) {

    global["Long"] = factory();

})(this, function() {
    "use strict";

    function Long(low, high, unsigned) {
        this.low = low | 0;
        this.high = high | 0;
        this.unsigned = !! unsigned;
    }
    Long.prototype.__isLong__;

    Object.defineProperty(Long.prototype, "__isLong__", {
        value: true,
        enumerable: false,
        configurable: false
    });

    function isLong(obj) {
        return (obj && obj["__isLong__"]) === true;
    }
    Long.isLong = isLong;
    var INT_CACHE = {};
    var UINT_CACHE = {};

    function fromInt(value, unsigned) {
        var obj, cachedObj, cache;
        if (unsigned) {
            value >>>= 0;
            if (cache = (0 <= value && value < 256)) {
                cachedObj = UINT_CACHE[value];
                if (cachedObj)
                    return cachedObj;
            }
            obj = fromBits(value, (value | 0) < 0 ? -1 : 0, true);
            if (cache)
                UINT_CACHE[value] = obj;
            return obj;
        } else {
            value |= 0;
            if (cache = (-128 <= value && value < 128)) {
                cachedObj = INT_CACHE[value];
                if (cachedObj)
                    return cachedObj;
            }
            obj = fromBits(value, value < 0 ? -1 : 0, false);
            if (cache)
                INT_CACHE[value] = obj;
            return obj;
        }
    }
    Long.fromInt = fromInt;

    function fromNumber(value, unsigned) {
        if (isNaN(value) || !isFinite(value))
            return unsigned ? UZERO : ZERO;
        if (unsigned) {
            if (value < 0)
                return UZERO;
            if (value >= TWO_PWR_64_DBL)
                return MAX_UNSIGNED_VALUE;
        } else {
            if (value <= -TWO_PWR_63_DBL)
                return MIN_VALUE;
            if (value + 1 >= TWO_PWR_63_DBL)
                return MAX_VALUE;
        }
        if (value < 0)
            return fromNumber(-value, unsigned).neg();
        return fromBits((value % TWO_PWR_32_DBL) | 0, (value / TWO_PWR_32_DBL) | 0, unsigned);
    }
    Long.fromNumber = fromNumber;

    function fromBits(lowBits, highBits, unsigned) {
        return new Long(lowBits, highBits, unsigned);
    }
    Long.fromBits = fromBits;
    var pow_dbl = Math.pow; // Used 4 times (4*8 to 15+4)

    function fromString(str, unsigned, radix) {
        if (str.length === 0)
            throw Error('empty string');
        if (str === "NaN" || str === "Infinity" || str === "+Infinity" || str === "-Infinity")
            return ZERO;
        if (typeof unsigned === 'number') {
            // For goog.math.long compatibility
            radix = unsigned,
            unsigned = false;
        } else {
            unsigned = !! unsigned;
        }
        radix = radix || 10;
        if (radix < 2 || 36 < radix)
            throw RangeError('radix');

        var p;
        if ((p = str.indexOf('-')) > 0)
            throw Error('interior hyphen');
        else if (p === 0) {
            return fromString(str.substring(1), unsigned, radix).neg();
        }
        var radixToPower = fromNumber(pow_dbl(radix, 8));

        var result = ZERO;
        for (var i = 0; i < str.length; i += 8) {
            var size = Math.min(8, str.length - i),
                value = parseInt(str.substring(i, i + size), radix);
            if (size < 8) {
                var power = fromNumber(pow_dbl(radix, size));
                result = result.mul(power).add(fromNumber(value));
            } else {
                result = result.mul(radixToPower);
                result = result.add(fromNumber(value));
            }
        }
        result.unsigned = unsigned;
        return result;
    }
    Long.fromString = fromString;

    function fromValue(val) {
        if (val /* is compatible */ instanceof Long)
            return val;
        if (typeof val === 'number')
            return fromNumber(val);
        if (typeof val === 'string')
            return fromString(val);
        // Throws for non-objects, converts non-instanceof Long:
        return fromBits(val.low, val.high, val.unsigned);
    }
    Long.fromValue = fromValue;
    var TWO_PWR_16_DBL = 1 << 16;
    var TWO_PWR_24_DBL = 1 << 24;
    var TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL;
    var TWO_PWR_64_DBL = TWO_PWR_32_DBL * TWO_PWR_32_DBL;
    var TWO_PWR_63_DBL = TWO_PWR_64_DBL / 2;
    var TWO_PWR_24 = fromInt(TWO_PWR_24_DBL);
    var ZERO = fromInt(0);
    Long.ZERO = ZERO;
    var UZERO = fromInt(0, true);
    Long.UZERO = UZERO;
    var ONE = fromInt(1);
    Long.ONE = ONE;
    var UONE = fromInt(1, true);
    Long.UONE = UONE;
    var NEG_ONE = fromInt(-1);
    Long.NEG_ONE = NEG_ONE;
    var MAX_VALUE = fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0, false);
    Long.MAX_VALUE = MAX_VALUE;
    var MAX_UNSIGNED_VALUE = fromBits(0xFFFFFFFF | 0, 0xFFFFFFFF | 0, true);
    Long.MAX_UNSIGNED_VALUE = MAX_UNSIGNED_VALUE;
    var MIN_VALUE = fromBits(0, 0x80000000 | 0, false);
    Long.MIN_VALUE = MIN_VALUE;
    var LongPrototype = Long.prototype;
    LongPrototype.toInt = function toInt() {
        return this.unsigned ? this.low >>> 0 : this.low;
    };
    LongPrototype.toNumber = function toNumber() {
        if (this.unsigned)
            return ((this.high >>> 0) * TWO_PWR_32_DBL) + (this.low >>> 0);
        return this.high * TWO_PWR_32_DBL + (this.low >>> 0);
    };

    LongPrototype.toString = function toString(radix) {
        radix = radix || 10;
        if (radix < 2 || 36 < radix)
            throw RangeError('radix');
        if (this.isZero())
            return '0';
        if (this.isNegative()) { // Unsigned Longs are never negative
            if (this.eq(MIN_VALUE)) {
                // We need to change the Long value before it can be negated, so we remove
                // the bottom-most digit in this base and then recurse to do the rest.
                var radixLong = fromNumber(radix),
                    div = this.div(radixLong),
                    rem1 = div.mul(radixLong).sub(this);
                return div.toString(radix) + rem1.toInt().toString(radix);
            } else
                return '-' + this.neg().toString(radix);
        }
        var radixToPower = fromNumber(pow_dbl(radix, 6), this.unsigned),
            rem = this;
        var result = '';
        while (true) {
            var remDiv = rem.div(radixToPower),
                intval = rem.sub(remDiv.mul(radixToPower)).toInt() >>> 0,
                digits = intval.toString(radix);
            rem = remDiv;
            if (rem.isZero())
                return digits + result;
            else {
                while (digits.length < 6)
                    digits = '0' + digits;
                result = '' + digits + result;
            }
        }
    };
    LongPrototype.getHighBits = function getHighBits() {
        return this.high;
    };
    LongPrototype.getHighBitsUnsigned = function getHighBitsUnsigned() {
        return this.high >>> 0;
    };
    LongPrototype.getLowBits = function getLowBits() {
        return this.low;
    };
    LongPrototype.getLowBitsUnsigned = function getLowBitsUnsigned() {
        return this.low >>> 0;
    };
    LongPrototype.getNumBitsAbs = function getNumBitsAbs() {
        if (this.isNegative()) // Unsigned Longs are never negative
            return this.eq(MIN_VALUE) ? 64 : this.neg().getNumBitsAbs();
        var val = this.high != 0 ? this.high : this.low;
        for (var bit = 31; bit > 0; bit--)
            if ((val & (1 << bit)) != 0)
                break;
        return this.high != 0 ? bit + 33 : bit + 1;
    };
    LongPrototype.isZero = function isZero() {
        return this.high === 0 && this.low === 0;
    };
    LongPrototype.isNegative = function isNegative() {
        return !this.unsigned && this.high < 0;
    };
    LongPrototype.isPositive = function isPositive() {
        return this.unsigned || this.high >= 0;
    };
    LongPrototype.isOdd = function isOdd() {
        return (this.low & 1) === 1;
    };
    LongPrototype.isEven = function isEven() {
        return (this.low & 1) === 0;
    };
    LongPrototype.equals = function equals(other) {
        if (!isLong(other))
            other = fromValue(other);
        if (this.unsigned !== other.unsigned && (this.high >>> 31) === 1 && (other.high >>> 31) === 1)
            return false;
        return this.high === other.high && this.low === other.low;
    };
    LongPrototype.eq = LongPrototype.equals;
    LongPrototype.notEquals = function notEquals(other) {
        return !this.eq( /* validates */ other);
    };
    LongPrototype.neq = LongPrototype.notEquals;

    /**
     * Tests if this Long's value is less than the specified's.
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    LongPrototype.lessThan = function lessThan(other) {
        return this.comp( /* validates */ other) < 0;
    };

    /**
     * Tests if this Long's value is less than the specified's. This is an alias of {@link Long#lessThan}.
     * @function
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    LongPrototype.lt = LongPrototype.lessThan;

    /**
     * Tests if this Long's value is less than or equal the specified's.
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    LongPrototype.lessThanOrEqual = function lessThanOrEqual(other) {
        return this.comp( /* validates */ other) <= 0;
    };

    /**
     * Tests if this Long's value is less than or equal the specified's. This is an alias of {@link Long#lessThanOrEqual}.
     * @function
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    LongPrototype.lte = LongPrototype.lessThanOrEqual;

    /**
     * Tests if this Long's value is greater than the specified's.
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    LongPrototype.greaterThan = function greaterThan(other) {
        return this.comp( /* validates */ other) > 0;
    };

    /**
     * Tests if this Long's value is greater than the specified's. This is an alias of {@link Long#greaterThan}.
     * @function
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    LongPrototype.gt = LongPrototype.greaterThan;

    /**
     * Tests if this Long's value is greater than or equal the specified's.
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    LongPrototype.greaterThanOrEqual = function greaterThanOrEqual(other) {
        return this.comp( /* validates */ other) >= 0;
    };

    /**
     * Tests if this Long's value is greater than or equal the specified's. This is an alias of {@link Long#greaterThanOrEqual}.
     * @function
     * @param {!Long|number|string} other Other value
     * @returns {boolean}
     */
    LongPrototype.gte = LongPrototype.greaterThanOrEqual;

    /**
     * Compares this Long's value with the specified's.
     * @param {!Long|number|string} other Other value
     * @returns {number} 0 if they are the same, 1 if the this is greater and -1
     *  if the given one is greater
     */
    LongPrototype.compare = function compare(other) {
        if (!isLong(other))
            other = fromValue(other);
        if (this.eq(other))
            return 0;
        var thisNeg = this.isNegative(),
            otherNeg = other.isNegative();
        if (thisNeg && !otherNeg)
            return -1;
        if (!thisNeg && otherNeg)
            return 1;
        // At this point the sign bits are the same
        if (!this.unsigned)
            return this.sub(other).isNegative() ? -1 : 1;
        // Both are positive if at least one is unsigned
        return (other.high >>> 0) > (this.high >>> 0) || (other.high === this.high && (other.low >>> 0) > (this.low >>> 0)) ? -1 : 1;
    };

    /**
     * Compares this Long's value with the specified's. This is an alias of {@link Long#compare}.
     * @function
     * @param {!Long|number|string} other Other value
     * @returns {number} 0 if they are the same, 1 if the this is greater and -1
     *  if the given one is greater
     */
    LongPrototype.comp = LongPrototype.compare;

    /**
     * Negates this Long's value.
     * @returns {!Long} Negated Long
     */
    LongPrototype.negate = function negate() {
        if (!this.unsigned && this.eq(MIN_VALUE))
            return MIN_VALUE;
        return this.not().add(ONE);
    };

    /**
     * Negates this Long's value. This is an alias of {@link Long#negate}.
     * @function
     * @returns {!Long} Negated Long
     */
    LongPrototype.neg = LongPrototype.negate;

    /**
     * Returns the sum of this and the specified Long.
     * @param {!Long|number|string} addend Addend
     * @returns {!Long} Sum
     */
    LongPrototype.add = function add(addend) {
        if (!isLong(addend))
            addend = fromValue(addend);

        // Divide each number into 4 chunks of 16 bits, and then sum the chunks.

        var a48 = this.high >>> 16;
        var a32 = this.high & 0xFFFF;
        var a16 = this.low >>> 16;
        var a00 = this.low & 0xFFFF;

        var b48 = addend.high >>> 16;
        var b32 = addend.high & 0xFFFF;
        var b16 = addend.low >>> 16;
        var b00 = addend.low & 0xFFFF;

        var c48 = 0,
            c32 = 0,
            c16 = 0,
            c00 = 0;
        c00 += a00 + b00;
        c16 += c00 >>> 16;
        c00 &= 0xFFFF;
        c16 += a16 + b16;
        c32 += c16 >>> 16;
        c16 &= 0xFFFF;
        c32 += a32 + b32;
        c48 += c32 >>> 16;
        c32 &= 0xFFFF;
        c48 += a48 + b48;
        c48 &= 0xFFFF;
        return fromBits((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
    };

    /**
     * Returns the difference of this and the specified Long.
     * @param {!Long|number|string} subtrahend Subtrahend
     * @returns {!Long} Difference
     */
    LongPrototype.subtract = function subtract(subtrahend) {
        if (!isLong(subtrahend))
            subtrahend = fromValue(subtrahend);
        return this.add(subtrahend.neg());
    };

    /**
     * Returns the difference of this and the specified Long. This is an alias of {@link Long#subtract}.
     * @function
     * @param {!Long|number|string} subtrahend Subtrahend
     * @returns {!Long} Difference
     */
    LongPrototype.sub = LongPrototype.subtract;

    /**
     * Returns the product of this and the specified Long.
     * @param {!Long|number|string} multiplier Multiplier
     * @returns {!Long} Product
     */
    LongPrototype.multiply = function multiply(multiplier) {
        if (this.isZero())
            return ZERO;
        if (!isLong(multiplier))
            multiplier = fromValue(multiplier);
        if (multiplier.isZero())
            return ZERO;
        if (this.eq(MIN_VALUE))
            return multiplier.isOdd() ? MIN_VALUE : ZERO;
        if (multiplier.eq(MIN_VALUE))
            return this.isOdd() ? MIN_VALUE : ZERO;

        if (this.isNegative()) {
            if (multiplier.isNegative())
                return this.neg().mul(multiplier.neg());
            else
                return this.neg().mul(multiplier).neg();
        } else if (multiplier.isNegative())
            return this.mul(multiplier.neg()).neg();

        // If both longs are small, use float multiplication
        if (this.lt(TWO_PWR_24) && multiplier.lt(TWO_PWR_24))
            return fromNumber(this.toNumber() * multiplier.toNumber(), this.unsigned);

        // Divide each long into 4 chunks of 16 bits, and then add up 4x4 products.
        // We can skip products that would overflow.

        var a48 = this.high >>> 16;
        var a32 = this.high & 0xFFFF;
        var a16 = this.low >>> 16;
        var a00 = this.low & 0xFFFF;

        var b48 = multiplier.high >>> 16;
        var b32 = multiplier.high & 0xFFFF;
        var b16 = multiplier.low >>> 16;
        var b00 = multiplier.low & 0xFFFF;

        var c48 = 0,
            c32 = 0,
            c16 = 0,
            c00 = 0;
        c00 += a00 * b00;
        c16 += c00 >>> 16;
        c00 &= 0xFFFF;
        c16 += a16 * b00;
        c32 += c16 >>> 16;
        c16 &= 0xFFFF;
        c16 += a00 * b16;
        c32 += c16 >>> 16;
        c16 &= 0xFFFF;
        c32 += a32 * b00;
        c48 += c32 >>> 16;
        c32 &= 0xFFFF;
        c32 += a16 * b16;
        c48 += c32 >>> 16;
        c32 &= 0xFFFF;
        c32 += a00 * b32;
        c48 += c32 >>> 16;
        c32 &= 0xFFFF;
        c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
        c48 &= 0xFFFF;
        return fromBits((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
    };

    /**
     * Returns the product of this and the specified Long. This is an alias of {@link Long#multiply}.
     * @function
     * @param {!Long|number|string} multiplier Multiplier
     * @returns {!Long} Product
     */
    LongPrototype.mul = LongPrototype.multiply;

    /**
     * Returns this Long divided by the specified. The result is signed if this Long is signed or
     *  unsigned if this Long is unsigned.
     * @param {!Long|number|string} divisor Divisor
     * @returns {!Long} Quotient
     */
    LongPrototype.divide = function divide(divisor) {
        if (!isLong(divisor))
            divisor = fromValue(divisor);
        if (divisor.isZero())
            throw Error('division by zero');
        if (this.isZero())
            return this.unsigned ? UZERO : ZERO;
        var approx, rem, res;
        if (!this.unsigned) {
            // This section is only relevant for signed longs and is derived from the
            // closure library as a whole.
            if (this.eq(MIN_VALUE)) {
                if (divisor.eq(ONE) || divisor.eq(NEG_ONE))
                    return MIN_VALUE; // recall that -MIN_VALUE == MIN_VALUE
                else if (divisor.eq(MIN_VALUE))
                    return ONE;
                else {
                    // At this point, we have |other| >= 2, so |this/other| < |MIN_VALUE|.
                    var halfThis = this.shr(1);
                    approx = halfThis.div(divisor).shl(1);
                    if (approx.eq(ZERO)) {
                        return divisor.isNegative() ? ONE : NEG_ONE;
                    } else {
                        rem = this.sub(divisor.mul(approx));
                        res = approx.add(rem.div(divisor));
                        return res;
                    }
                }
            } else if (divisor.eq(MIN_VALUE))
                return this.unsigned ? UZERO : ZERO;
            if (this.isNegative()) {
                if (divisor.isNegative())
                    return this.neg().div(divisor.neg());
                return this.neg().div(divisor).neg();
            } else if (divisor.isNegative())
                return this.div(divisor.neg()).neg();
            res = ZERO;
        } else {
            // The algorithm below has not been made for unsigned longs. It's therefore
            // required to take special care of the MSB prior to running it.
            if (!divisor.unsigned)
                divisor = divisor.toUnsigned();
            if (divisor.gt(this))
                return UZERO;
            if (divisor.gt(this.shru(1))) // 15 >>> 1 = 7 ; with divisor = 8 ; true
                return UONE;
            res = UZERO;
        }

        // Repeat the following until the remainder is less than other:  find a
        // floating-point that approximates remainder / other *from below*, add this
        // into the result, and subtract it from the remainder.  It is critical that
        // the approximate value is less than or equal to the real value so that the
        // remainder never becomes negative.
        rem = this;
        while (rem.gte(divisor)) {
            // Approximate the result of division. This may be a little greater or
            // smaller than the actual value.
            approx = Math.max(1, Math.floor(rem.toNumber() / divisor.toNumber()));

            // We will tweak the approximate result by changing it in the 48-th digit or
            // the smallest non-fractional digit, whichever is larger.
            var log2 = Math.ceil(Math.log(approx) / Math.LN2),
                delta = (log2 <= 48) ? 1 : pow_dbl(2, log2 - 48),

                // Decrease the approximation until it is smaller than the remainder.  Note
                // that if it is too large, the product overflows and is negative.
                approxRes = fromNumber(approx),
                approxRem = approxRes.mul(divisor);
            while (approxRem.isNegative() || approxRem.gt(rem)) {
                approx -= delta;
                approxRes = fromNumber(approx, this.unsigned);
                approxRem = approxRes.mul(divisor);
            }

            // We know the answer can't be zero... and actually, zero would cause
            // infinite recursion since we would make no progress.
            if (approxRes.isZero())
                approxRes = ONE;

            res = res.add(approxRes);
            rem = rem.sub(approxRem);
        }
        return res;
    };

    /**
     * Returns this Long divided by the specified. This is an alias of {@link Long#divide}.
     * @function
     * @param {!Long|number|string} divisor Divisor
     * @returns {!Long} Quotient
     */
    LongPrototype.div = LongPrototype.divide;

    /**
     * Returns this Long modulo the specified.
     * @param {!Long|number|string} divisor Divisor
     * @returns {!Long} Remainder
     */
    LongPrototype.modulo = function modulo(divisor) {
        if (!isLong(divisor))
            divisor = fromValue(divisor);
        return this.sub(this.div(divisor).mul(divisor));
    };

    /**
     * Returns this Long modulo the specified. This is an alias of {@link Long#modulo}.
     * @function
     * @param {!Long|number|string} divisor Divisor
     * @returns {!Long} Remainder
     */
    LongPrototype.mod = LongPrototype.modulo;

    /**
     * Returns the bitwise NOT of this Long.
     * @returns {!Long}
     */
    LongPrototype.not = function not() {
        return fromBits(~this.low, ~this.high, this.unsigned);
    };

    /**
     * Returns the bitwise AND of this Long and the specified.
     * @param {!Long|number|string} other Other Long
     * @returns {!Long}
     */
    LongPrototype.and = function and(other) {
        if (!isLong(other))
            other = fromValue(other);
        return fromBits(this.low & other.low, this.high & other.high, this.unsigned);
    };

    /**
     * Returns the bitwise OR of this Long and the specified.
     * @param {!Long|number|string} other Other Long
     * @returns {!Long}
     */
    LongPrototype.or = function or(other) {
        if (!isLong(other))
            other = fromValue(other);
        return fromBits(this.low | other.low, this.high | other.high, this.unsigned);
    };

    /**
     * Returns the bitwise XOR of this Long and the given one.
     * @param {!Long|number|string} other Other Long
     * @returns {!Long}
     */
    LongPrototype.xor = function xor(other) {
        if (!isLong(other))
            other = fromValue(other);
        return fromBits(this.low ^ other.low, this.high ^ other.high, this.unsigned);
    };

    /**
     * Returns this Long with bits shifted to the left by the given amount.
     * @param {number|!Long} numBits Number of bits
     * @returns {!Long} Shifted Long
     */
    LongPrototype.shiftLeft = function shiftLeft(numBits) {
        if (isLong(numBits))
            numBits = numBits.toInt();
        if ((numBits &= 63) === 0)
            return this;
        else if (numBits < 32)
            return fromBits(this.low << numBits, (this.high << numBits) | (this.low >>> (32 - numBits)), this.unsigned);
        else
            return fromBits(0, this.low << (numBits - 32), this.unsigned);
    };

    /**
     * Returns this Long with bits shifted to the left by the given amount. This is an alias of {@link Long#shiftLeft}.
     * @function
     * @param {number|!Long} numBits Number of bits
     * @returns {!Long} Shifted Long
     */
    LongPrototype.shl = LongPrototype.shiftLeft;

    /**
     * Returns this Long with bits arithmetically shifted to the right by the given amount.
     * @param {number|!Long} numBits Number of bits
     * @returns {!Long} Shifted Long
     */
    LongPrototype.shiftRight = function shiftRight(numBits) {
        if (isLong(numBits))
            numBits = numBits.toInt();
        if ((numBits &= 63) === 0)
            return this;
        else if (numBits < 32)
            return fromBits((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >> numBits, this.unsigned);
        else
            return fromBits(this.high >> (numBits - 32), this.high >= 0 ? 0 : -1, this.unsigned);
    };

    /**
     * Returns this Long with bits arithmetically shifted to the right by the given amount. This is an alias of {@link Long#shiftRight}.
     * @function
     * @param {number|!Long} numBits Number of bits
     * @returns {!Long} Shifted Long
     */
    LongPrototype.shr = LongPrototype.shiftRight;

    /**
     * Returns this Long with bits logically shifted to the right by the given amount.
     * @param {number|!Long} numBits Number of bits
     * @returns {!Long} Shifted Long
     */
    LongPrototype.shiftRightUnsigned = function shiftRightUnsigned(numBits) {
        if (isLong(numBits))
            numBits = numBits.toInt();
        numBits &= 63;
        if (numBits === 0)
            return this;
        else {
            var high = this.high;
            if (numBits < 32) {
                var low = this.low;
                return fromBits((low >>> numBits) | (high << (32 - numBits)), high >>> numBits, this.unsigned);
            } else if (numBits === 32)
                return fromBits(high, 0, this.unsigned);
            else
                return fromBits(high >>> (numBits - 32), 0, this.unsigned);
        }
    };

    /**
     * Returns this Long with bits logically shifted to the right by the given amount. This is an alias of {@link Long#shiftRightUnsigned}.
     * @function
     * @param {number|!Long} numBits Number of bits
     * @returns {!Long} Shifted Long
     */
    LongPrototype.shru = LongPrototype.shiftRightUnsigned;

    /**
     * Converts this Long to signed.
     * @returns {!Long} Signed long
     */
    LongPrototype.toSigned = function toSigned() {
        if (!this.unsigned)
            return this;
        return fromBits(this.low, this.high, false);
    };

    /**
     * Converts this Long to unsigned.
     * @returns {!Long} Unsigned long
     */
    LongPrototype.toUnsigned = function toUnsigned() {
        if (this.unsigned)
            return this;
        return fromBits(this.low, this.high, true);
    };

    /**
     * Converts this Long to its byte representation.
     * @param {boolean=} le Whether little or big endian, defaults to big endian
     * @returns {!Array.<number>} Byte representation
     */
    LongPrototype.toBytes = function(le) {
        return le ? this.toBytesLE() : this.toBytesBE();
    }

    /**
     * Converts this Long to its little endian byte representation.
     * @returns {!Array.<number>} Little endian byte representation
     */
    LongPrototype.toBytesLE = function() {
        var hi = this.high,
            lo = this.low;
        return [
            lo & 0xff, (lo >>> 8) & 0xff, (lo >>> 16) & 0xff, (lo >>> 24) & 0xff,
            hi & 0xff, (hi >>> 8) & 0xff, (hi >>> 16) & 0xff, (hi >>> 24) & 0xff
        ];
    }

    /**
     * Converts this Long to its big endian byte representation.
     * @returns {!Array.<number>} Big endian byte representation
     */
    LongPrototype.toBytesBE = function() {
        var hi = this.high,
            lo = this.low;
        return [
            (hi >>> 24) & 0xff, (hi >>> 16) & 0xff, (hi >>> 8) & 0xff,
            hi & 0xff, (lo >>> 24) & 0xff, (lo >>> 16) & 0xff, (lo >>> 8) & 0xff,
            lo & 0xff
        ];
    }

    return Long;
});



/* webim javascript SDK
 * VER 1.7.0
 */

/* webim API definitions
 */
var webim = { // namespace object webim

    /* function init
     *   sdk登录
     * params:
     *   loginInfo      - Object, 登录身份相关参数集合，详见下面
     *   {
     *     sdkAppID     - String, 用户标识接入SDK的应用ID，必填
     *     identifier   - String, 用户帐号,必须是字符串类型，必填
     *     accountType  - int, 账号类型，必填
     *     identifierNick   - String, 用户昵称，选填
     *     userSig      - String, 鉴权Token，必须是字符串类型，必填
     *   }
     *   listeners      - Object, 事件回调函数集合, 详见下面
     *   {
     *     onConnNotify - function(connInfo), 用于收到连接状态相关通知的回调函数,目前未使用
     *     jsonpCallback -function(rspData),//IE9(含)以下浏览器用到的jsonp回调函数
     *     onMsgNotify  - function(newMsgList), 用于收到消息通知的回调函数,
     *      newMsgList为新消息数组，格式为[Msg对象]
     *      使用方有两种处理回调: 1)处理newMsgList中的增量消息,2)直接访问webim.MsgStore获取最新的消息
     *     onGroupInfoChangeNotify  - function(groupInfo), 用于监听群组资料变更的回调函数,
     *          groupInfo为新的群组资料信息
     *     onGroupSystemNotifys - Object, 用于监听（多终端同步）群系统消息的回调函数对象
     *
     *   }
     *   options        - Object, 其它选项, 目前未使用
     * return:
     *   (无)
     */
    login: function(loginInfo, listeners, options) {},

    /* function syncMsgs
     *   拉取最新C2C消息
     *   一般不需要使用方直接调用, SDK底层会自动同步最新消息并通知使用方, 一种有用的调用场景是用户手动触发刷新消息
     * params:
     *   cbOk   - function(msgList)类型, 当同步消息成功时的回调函数, msgList为新消息数组，格式为[Msg对象],
     *            如果此参数为null或undefined则同步消息成功后会像自动同步那样回调cbNotify
     *   cbErr  - function(err)类型, 当同步消息失败时的回调函数, err为错误对象
     * return:
     *   (无)
     */
    syncMsgs: function(cbOk, cbErr) {},


    /* function getC2CHistoryMsgs
     * 拉取C2C漫游消息
     * params:
     *   options    - 请求参数
     *   cbOk   - function(msgList)类型, 成功时的回调函数, msgList为消息数组，格式为[Msg对象],
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    getC2CHistoryMsgs: function(options, cbOk, cbErr) {},

    /* function syncGroupMsgs
     * 拉取群漫游消息
     * params:
     *   options    - 请求参数
     *   cbOk   - function(msgList)类型, 成功时的回调函数, msgList为消息数组，格式为[Msg对象],
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    syncGroupMsgs: function(options, cbOk, cbErr) {},

    /* function sendMsg
     *   发送一条消息
     * params:
     *   msg    - webim.Msg类型, 要发送的消息对象
     *   cbOk   - function()类型, 当发送消息成功时的回调函数
     *   cbErr  - function(err)类型, 当发送消息失败时的回调函数, err为错误对象
     * return:
     *   (无)
     */
    sendMsg: function(msg, cbOk, cbErr) {},

    /* function logout
     *   sdk登出
     * params:
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    logout: function(cbOk, cbErr) {},

    /* function setAutoRead
     * 设置会话自动已读上报标志
     * params:
     *   selSess    - webim.Session类型, 当前会话
     *   isOn   - boolean, 将selSess的自动已读消息标志改为isOn，同时是否上报当前会话已读消息
     *   isResetAll - boolean，是否重置所有会话的自动已读标志
     * return:
     *   (无)
     */
    setAutoRead: function(selSess, isOn, isResetAll) {},

    /* function getProfilePortrait
     *   拉取资料（搜索用户）
     * params:
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    getProfilePortrait: function(options, cbOk, cbErr) {},

    /* function setProfilePortrait
     *   设置个人资料
     * params:
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    setProfilePortrait: function(options, cbOk, cbErr) {},

    /* function applyAddFriend
     *   申请添加好友
     * params:
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    applyAddFriend: function(options, cbOk, cbErr) {},

    /* function getPendency
     *   拉取好友申请
     * params:
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    getPendency: function(options, cbOk, cbErr) {},

    /* function deletePendency
     *   删除好友申请
     * params:
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    deletePendency: function(options, cbOk, cbErr) {},

    /* function responseFriend
     *   响应好友申请
     * params:
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    responseFriend: function(options, cbOk, cbErr) {},

    /* function getAllFriend
     *   拉取我的好友
     * params:
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    getAllFriend: function(options, cbOk, cbErr) {},

    /* function deleteFriend
     *   删除好友
     * params:
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    deleteFriend: function(options, cbOk, cbErr) {},

    /* function addBlackList
     *   增加黑名单
     * params:
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    addBlackList: function(options, cbOk, cbErr) {},

    /* function getBlackList
     *   删除黑名单
     * params:
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    getBlackList: function(options, cbOk, cbErr) {},

    /* function deleteBlackList
     *   我的黑名单
     * params:
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    deleteBlackList: function(options, cbOk, cbErr) {},

    /* function uploadPic
     *   上传图片
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    uploadPic: function(options, cbOk, cbErr) {},

    /* function createGroup
     *   创建群
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    createGroup: function(options, cbOk, cbErr) {},

    /* function applyJoinGroup
     *   申请加群
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    applyJoinGroup: function(options, cbOk, cbErr) {},

    /* function handleApplyJoinGroup
     *   处理申请加群(同意或拒绝)
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    handleApplyJoinGroup: function(options, cbOk, cbErr) {},

    /* function deleteApplyJoinGroupPendency
     *   删除加群申请
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    deleteApplyJoinGroupPendency: function(options, cbOk, cbErr) {},


    /* function quitGroup
     *  主动退群
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    quitGroup: function(options, cbOk, cbErr) {},

    /* function getGroupPublicInfo
     *   读取群公开资料-高级接口
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    getGroupPublicInfo: function(options, cbOk, cbErr) {},

    /* function getGroupInfo
     *   读取群详细资料-高级接口
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    getGroupInfo: function(options, cbOk, cbErr) {},

    /* function modifyGroupBaseInfo
     *   修改群基本资料
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    modifyGroupBaseInfo: function(options, cbOk, cbErr) {},

    /* function destroyGroup
     *  解散群
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    destroyGroup: function(options, cbOk, cbErr) {},

    /* function getJoinedGroupListHigh
     *   获取我的群组-高级接口
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    getJoinedGroupListHigh: function(options, cbOk, cbErr) {},

    /* function getGroupMemberInfo
     *   获取群组成员列表
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    getGroupMemberInfo: function(options, cbOk, cbErr) {},

    /* function addGroupMember
     *   邀请好友加群
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    addGroupMember: function(options, cbOk, cbErr) {},

    /* function modifyGroupMember
     *   修改群成员资料（角色或者群消息提类型示）
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    modifyGroupMember: function(options, cbOk, cbErr) {},

    /* function forbidSendMsg
     *   设置群成员禁言时间
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    forbidSendMsg: function(options, cbOk, cbErr) {},

    /* function deleteGroupMember
     *   删除群成员
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    deleteGroupMember: function(options, cbOk, cbErr) {},

    /* function getPendencyGroup
     *   获取群组未决列表
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    getPendencyGroup: function(options, cbOk, cbErr) {},

    /* function getPendencyReport
     *   好友未决已读上报
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    getPendencyReport: function(options, cbOk, cbErr) {},

    /* function getPendencyGroupRead
     *   群组未决已读上报
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    getPendencyGroupRead: function(options, cbOk, cbErr) {},

    /* function sendCustomGroupNotify
     *   发送自定义群通知
     * params:
     *   options    - 请求参数，详见api文档
     *   cbOk   - function()类型, 成功时回调函数
     *   cbErr  - function(err)类型, 失败时回调函数, err为错误对象
     * return:
     *   (无)
     */
    sendCustomGroupNotify: function(options, cbOk, cbErr) {},

    /* class webim.Msg
     *   一条消息的描述类, 消息发送、接收的API中都会涉及此类型的对象
     * properties:
     *   sess   - Session object-ref, 消息所属的会话(e.g:我与好友A的C2C会话，我与群组G的GROUP会话)
     *   isSend - Boolean, true表示是我发出消息, false表示是发给我的消息)
     *   seq    - Integer, 消息序列号, 用于判断消息是否同一条
     *   random - Integer, 消息随机数,用于判断消息是否同一条
     *   time   - Integer, 消息时间戳, 为unix timestamp
     *   fromAccount -String,  消息发送者帐号
     *   subType -Integer,  消息子类型，c2c消息时，0-表示普通消息；群消息时，0-普通消息，1-点赞消息，2-提示消息
     *   fromAccountNick -String,  消息发送者昵称
     *   elems  - Array of webim.Msg.Elem, 描述消息内容的元素列表
     * constructor:
     *   Msg(sess, isSend, seq,random time,fromAccount) - 构造函数, 参数定义同上面properties中定义
     * methods:
     *   addText(text)  - 向elems中添加一个TEXT元素
     *   addFace(face)  - 向elems中添加一个FACE元素
     *   toHtml()       - 转成可展示的html String
     *addFace
     * sub-class webim.Msg.Elem
     *   消息中一个组成元素的描述类, 一条消息的内容被抽象描述为N个元素的有序列表
     * properties:
     *   type   - 元素类型, 目前有TEXT(文本)、FACE(表情)、IMAGE(图片)等
     *   content- 元素内容体, 当TEXT时为String, 当PIC时为UrlString
     * constructor:
     *   Elem(type, content) - 构造函数, 参数定义同上面properties中定义
     *
     * sub-class webim.Msg.Elem.TextElem
     *   文本
     * properties:
     *   text  - String 内容
     * constructor:
     *   TextElem(text) - 构造函数, 参数定义同上面properties中定义
     *
     * sub-class webim.Msg.Elem.FaceElem
     *   表情
     * properties:
     *   index  - Integer 表情索引, 用户自定义
     *   data   - String 额外数据，用户自定义
     * constructor:
     *   FaceElem(index,data) - 构造函数, 参数定义同上面properties中定义
     *
     *
     */
    Msg: function(sess, isSend, seq, random, time, fromAccount, subType, fromAccountNick) { /*Class constructor*/ },

    /* singleton object MsgStore
     * webim.MsgStore是消息数据的Model对象(参考MVC概念), 它提供接口访问当前存储的会话和消息数据
     * 下面说明下会话数据类型: Session
     *
     * class Session
     *   一个Session对象描述一个会话，会话可简单理解为最近会话列表的一个条目，它由两个字段唯一标识:
     *     type - String, 会话类型(如"C2C", "GROUP", ...)
     *     id   - String, 会话ID(如C2C类型中为对方帐号,"C2C"时为好友ID,"GROUP"时为群ID)
     * properties:
     *   (Session对象未对外暴露任何属性字段, 所有访问通过下面的getter方法进行)
     * methods:
     *   type()     - String, 返回会话类型,"C2C"表示与好友私聊，"GROUP"表示群聊
     *   id()       - String, 返回会话ID
     *   name()     - String, 返回会话标题(如C2C类型中为对方的昵称,暂不支持)
     *   icon()     - String, 返回会话图标(对C2C类型中为对方的头像URL，暂不支持)
     *   unread()           - Integer, 返回会话未读条数
     *   time()     - Integer, 返回会话最后活跃时间, 为unix timestamp
     *   curMaxMsgSeq() - Integer, 返回会话最大消息序列号
     *   msgCount() - Integer, 返回会话中所有消息条数
     *   msg(index) - webim.Msg, 返回会话中第index条消息
     */
    MsgStore: {
        /* function sessMap
         *   获取所有会话
         * return:
         *   所有会话对象
         */
        sessMap: function() {
            return { /*Object*/ };
        },
        /* function sessCount
         *   获取当前会话的个数
         * return:
         *   Integer, 会话个数
         */
        sessCount: function() {
            return 0;
        },

        /* function sessByTypeId
         *   根据会话类型和会话ID取得相应会话
         * params:
         *   type   - String, 会话类型(如"C2C", "GROUP", ...)
         *   id     - String, 会话ID(如对方ID)
         * return:
         *   Session, 会话对象(说明见上面)
         */
        sessByTypeId: function(type, id) {
            return { /*Session Object*/ };
        },
        /* function delSessByTypeId
         *   根据会话类型和会话ID删除相应会话
         * params:
         *   type   - String, 会话类型(如"C2C", "GROUP", ...)
         *   id     - String, 会话ID(如对方ID)
         * return:
         *   Boolean, 布尔类型
         */
        delSessByTypeId: function(type, id) {
            return true;
        },

        /* function resetCookieAndSyncFlag
         *   重置上一次读取新c2c消息Cookie和是否继续拉取标记
         * return:
         *
         */
        resetCookieAndSyncFlag: function() {},

        downloadMap: {}
    }

};

/* webim API implementation
 */
(function(webim) {
    //sdk版本
    var SDK = {
        'VERSION': '1.7.0', //sdk版本号
        'APPID': '537048168', //web im sdk 版本 APPID
        'PLAATFORM': "10" //发送请求时判断其是来自web端的请求
    };

    //是否启用正式环境，默认启用
    var isAccessFormaEnvironment = true;
    // var isAccessFormaEnvironment = false;

    //后台接口主机
    var SRV_HOST = {
        'FORMAL': {
            'COMMON': 'https://webim.tim.qq.com',
            'PIC': 'https://pic.tim.qq.com'
        },
        'TEST': {
            'COMMON': 'https://test.tim.qq.com',
            'PIC': 'https://pic.tim.qq.com'
        }
    };

    //浏览器版本信息
    var BROWSER_INFO = {};
    //是否为ie9（含）以下
    var lowerBR = false;

    //服务名称
    var SRV_NAME = {
        'OPEN_IM': 'openim', //私聊（拉取未读c2c消息，长轮询，c2c消息已读上报等）服务名
        'GROUP': 'group_open_http_svc', //群组管理（拉取群消息，创建群，群成员管理，群消息已读上报等）服务名
        'FRIEND': 'sns', //关系链管理（好友管理，黑名单管理等）服务名
        'PROFILE': 'profile', //资料管理（查询，设置个人资料等）服务名
        'RECENT_CONTACT': 'recentcontact', //最近联系人服务名
        'PIC': 'openpic', //图片（或文件）服务名
        'BIG_GROUP': 'group_open_http_noauth_svc', //直播大群 群组管理（申请加大群）服务名
        'BIG_GROUP_LONG_POLLING': 'group_open_long_polling_http_noauth_svc', //直播大群 长轮询（拉取消息等）服务名
        'IM_OPEN_STAT': 'imopenstat', //质量上报，统计接口错误率
        'DEL_CHAT': 'recentcontact' //删除会话
    };

    //不同服务对应的版本号
    var SRV_NAME_VER = {
        'openim': 'v4',
        'group_open_http_svc': 'v4',
        'sns': 'v4',
        'profile': 'v4',
        'recentcontact': 'v4',
        'openpic': 'v4',
        'group_open_http_noauth_svc': 'v1',
        'group_open_long_polling_http_noauth_svc': 'v1',
        'imopenstat': 'v4',
        'recentcontact': 'v4'
    };

    //不同的命令名对应的上报类型ID，用于接口质量上报
    var CMD_EVENT_ID_MAP = {
        'login': 1, //登录
        'pic_up': 3, //上传图片
        'apply_join_group': 9, //申请加入群组
        'create_group': 10, //创建群组
        'longpolling': 18, //普通长轮询
        'send_group_msg': 19, //群聊
        'sendmsg': 20 //私聊
    };

    //聊天类型
    var SESSION_TYPE = {
        'C2C': 'C2C', //私聊
        'GROUP': 'GROUP' //群聊
    };

    //最近联系人类型
    var RECENT_CONTACT_TYPE = {
        'C2C': 1, //好友
        'GROUP': 2 //群
    };

    //消息最大长度（字节）
    var MSG_MAX_LENGTH = {
        'C2C': 12000, //私聊消息
        'GROUP': 8898 //群聊
    };

    //后台接口返回类型
    var ACTION_STATUS = {
        'OK': 'OK', //成功
        'FAIL': 'FAIL' //失败
    };

    var ERROR_CODE_CUSTOM = 99999; //自定义后台接口返回错误码

    //消息元素类型
    var MSG_ELEMENT_TYPE = {
        'TEXT': 'TIMTextElem', //文本
        'FACE': 'TIMFaceElem', //表情
        'IMAGE': 'TIMImageElem', //图片
        'CUSTOM': 'TIMCustomElem', //自定义
        'SOUND': 'TIMSoundElem', //语音,只支持显示
        'FILE': 'TIMFileElem', //文件,只支持显示
        'LOCATION': 'TIMLocationElem', //地理位置
        'GROUP_TIP': 'TIMGroupTipElem' //群提示消息,只支持显示
    };

    //图片类型
    var IMAGE_TYPE = {
        'ORIGIN': 1, //原图
        'LARGE': 2, //缩略大图
        'SMALL': 3 //缩略小图
    };

    //图片格式
    var IMAGE_FORMAT = {
        JPG: 0x1,
        JPEG: 0x1,
        GIF: 0x2,
        PNG: 0x3,
        BMP: 0x4,
        UNKNOWN: 0xff
    };


    //上传资源包类型
    var UPLOAD_RES_PKG_FLAG = {
        'RAW_DATA': 0, //原始数据
        'BASE64_DATA': 1 //base64编码数据
    };

    //下载文件配置
    var DOWNLOAD_FILE = {
        'BUSSINESS_ID': '10001', //下载文件业务ID
        'AUTH_KEY': '617574686b6579', //下载文件authkey
        'SERVER_IP': '182.140.186.147' //下载文件服务器IP
    };

    //下载文件类型
    var DOWNLOAD_FILE_TYPE = {
        "SOUND": 2106, //语音
        "FILE": 2107 //普通文件
    };

    //上传资源类型
    var UPLOAD_RES_TYPE = {
        "IMAGE": 1, //图片
        "FILE": 2, //文件
        "SHORT_VIDEO": 3, //短视频
        "SOUND": 4 //语音，PTT
    };

    //版本号，用于上传图片或文件接口
    var VERSION_INFO = {
        'APP_VERSION': '2.1', //应用版本号
        'SERVER_VERSION': 1 //服务端版本号
    };

    //长轮询消息类型
    var LONG_POLLINNG_EVENT_TYPE = {
        "C2C": 1 //新的c2c消息通知
        ,
        "GROUP_COMMON": 3 //新的群普通消息
        ,
        "GROUP_TIP": 4 //新的群提示消息
        ,
        "GROUP_SYSTEM": 5 //新的群系统消息
        ,
        "GROUP_TIP2": 6 //新的群提示消息2
        ,
        "FRIEND_NOTICE": 7 //好友系统通知
        ,
        "PROFILE_NOTICE": 8 //资料系统通知
        ,
        "C2C_COMMON": 9 //新的C2C消息
        ,
        "C2C_EVENT": 10

    };

    //c2c消息子类型
    var C2C_MSG_SUB_TYPE = {
        "COMMON": 0 //普通消息
    };
    //c2c消息子类型
    var C2C_EVENT_SUB_TYPE = {
        "READED": 92, //已读消息同步
        "KICKEDOUT": 96
    };

    //群消息子类型
    var GROUP_MSG_SUB_TYPE = {
        "COMMON": 0, //普通消息
        "LOVEMSG": 1, //点赞消息
        "TIP": 2, //提示消息
        "REDPACKET": 3 //红包消息
    };

    //群消息优先级类型
    var GROUP_MSG_PRIORITY_TYPE = {
        "REDPACKET": 1, //红包消息
        "COMMON": 2, //普通消息
        "LOVEMSG": 3 //点赞消息
    };

    //群提示消息类型
    var GROUP_TIP_TYPE = {
        "JOIN": 1, //加入群组
        "QUIT": 2, //退出群组
        "KICK": 3, //被踢出群组
        "SET_ADMIN": 4, //被设置为管理员
        "CANCEL_ADMIN": 5, //被取消管理员
        "MODIFY_GROUP_INFO": 6, //修改群资料
        "MODIFY_MEMBER_INFO": 7 //修改群成员信息
    };

    //群提示消息-群资料变更类型
    var GROUP_TIP_MODIFY_GROUP_INFO_TYPE = {
        "FACE_URL": 1, //修改群头像URL
        "NAME": 2, //修改群名称
        "OWNER": 3, //修改群主
        "NOTIFICATION": 4, //修改群公告
        "INTRODUCTION": 5 //修改群简介
    };

    //群系统消息类型
    var GROUP_SYSTEM_TYPE = {
        "JOIN_GROUP_REQUEST": 1, //申请加群请求（只有管理员会收到）
        "JOIN_GROUP_ACCEPT": 2, //申请加群被同意（只有申请人能够收到）
        "JOIN_GROUP_REFUSE": 3, //申请加群被拒绝（只有申请人能够收到）
        "KICK": 4, //被管理员踢出群(只有被踢者接收到)
        "DESTORY": 5, //群被解散(全员接收)
        "CREATE": 6, //创建群(创建者接收, 不展示)
        "INVITED_JOIN_GROUP_REQUEST": 7, //邀请加群(被邀请者接收)
        "QUIT": 8, //主动退群(主动退出者接收, 不展示)
        "SET_ADMIN": 9, //设置管理员(被设置者接收)
        "CANCEL_ADMIN": 10, //取消管理员(被取消者接收)
        "REVOKE": 11, //群已被回收(全员接收, 不展示)
        "READED": 15, //群消息已读同步
        "CUSTOM": 255, //用户自定义通知(默认全员接收)
        "INVITED_JOIN_GROUP_REQUEST_AGREE": 12, //邀请加群(被邀请者需同意)
    };

    //好友系统通知子类型
    var FRIEND_NOTICE_TYPE = {
        "FRIEND_ADD": 1, //好友表增加
        "FRIEND_DELETE": 2, //好友表删除
        "PENDENCY_ADD": 3, //未决增加
        "PENDENCY_DELETE": 4, //未决删除
        "BLACK_LIST_ADD": 5, //黑名单增加
        "BLACK_LIST_DELETE": 6, //黑名单删除
        "PENDENCY_REPORT": 7, //未决已读上报
        "FRIEND_UPDATE": 8 //好友数据更新
    };

    //资料系统通知子类型
    var PROFILE_NOTICE_TYPE = {
        "PROFILE_MODIFY": 1 //资料修改
    };

    //腾讯登录服务错误码（用于托管模式）
    var TLS_ERROR_CODE = {
        'OK': 0, //成功
        'SIGNATURE_EXPIRATION': 11 //用户身份凭证过期
    };

    //长轮询连接状态
    var CONNECTION_STATUS = {
        'INIT': -1, //初始化
        'ON': 0, //连接正常
        'RECONNECT': 1, //连接恢复正常
        'OFF': 9999 //连接已断开,可能是用户网络问题，或者长轮询接口报错引起的
    };

    var UPLOAD_PIC_BUSSINESS_TYPE = { //图片业务类型
        'GROUP_MSG': 1, //私聊图片
        'C2C_MSG': 2, //群聊图片
        'USER_HEAD': 3, //用户头像
        'GROUP_HEAD': 4 //群头像
    };

    var FRIEND_WRITE_MSG_ACTION = { //好友输入消息状态
        'ING': 14, //正在输入
        'STOP': 15 //停止输入
    };

    //ajax默认超时时间，单位：毫秒
    var ajaxDefaultTimeOut = 15000;

    //大群长轮询接口返回正常时，延时一定时间再发起下一次请求
    var OK_DELAY_TIME = 1000;

    //大群长轮询接口发生错误时，延时一定时间再发起下一次请求
    var ERROR_DELAY_TIME = 5000;

    //群提示消息最多显示人数
    var GROUP_TIP_MAX_USER_COUNT = 10;

    //长轮询连接状态
    var curLongPollingStatus = CONNECTION_STATUS.INIT;

    //当长轮询连接断开后，是否已经回调过
    var longPollingOffCallbackFlag = false;

    //当前长轮询返回错误次数
    var curLongPollingRetErrorCount = 0;

    //长轮询默认超时时间，单位：毫秒
    var longPollingDefaultTimeOut = 60000;

    //长轮询返回错误次数达到一定值后，发起新的长轮询请求间隔时间，单位：毫秒
    var longPollingIntervalTime = 5000;

    //没有新消息时，长轮询返回60008错误码是正常的
    var longPollingTimeOutErrorCode = 60008;

    //多实例登录被kick的错误码
    var longPollingKickedErrorCode = 91101;

    var LongPollingId = null;

    //当前大群长轮询返回错误次数
    var curBigGroupLongPollingRetErrorCount = 0;

    //最大允许长轮询返回错误次数
    var LONG_POLLING_MAX_RET_ERROR_COUNT = 10;

    //上传重试累计
    var Upload_Retry_Times = 0;
    //最大上传重试
    var Upload_Retry_Max_Times = 20;

    //ie7/8/9采用jsonp方法解决ajax跨域限制
    var jsonpRequestId = 0; //jsonp请求id
    //最新jsonp请求返回的json数据
    var jsonpLastRspData = null;
    //兼容ie7/8/9,jsonp回调函数
    var jsonpCallback = null;

    var uploadResultIframeId = 0; //用于上传图片的iframe id

    var ipList = []; //文件下载地址
    var authkey = null; //文件下载票据
    var expireTime = null; //文件下载票据超时时间

    //错误码
    var ERROR = {};
    //当前登录用户
    var ctx = {
        sdkAppID: null,
        appIDAt3rd: null,
        identifier: null,
        accountType: null,
        tinyid: null,
        identifierNick: null,
        userSig: null,
        a2: null,
        contentType: 'json',
        apn: 1
    };
    var opt = {};
    var xmlHttpObjSeq = 0; //ajax请求id
    var xmlHttpObjMap = {}; //发起的ajax请求
    var curSeq = 0; //消息seq
    var tempC2CMsgList = []; //新c2c消息临时缓存
    var tempC2CHistoryMsgList = []; //漫游c2c消息临时缓存

    var maxApiReportItemCount = 20; //一次最多上报条数
    var apiReportItems = []; //暂存api接口质量上报数据

    var Resources = {
        downloadMap: {}
    };

    //表情标识字符和索引映射关系对象，用户可以自定义
    var emotionDataIndexs = {
        "[你好]": 0,
        "[吃惊]": 1,
        "[尴尬]": 2,
        "[坏了]": 3,
        "[期待]": 4,
        "[生气]": 5,
        "[微笑]": 6,
        "[委屈]": 7,
        "[喜欢]": 8,
        "[心碎]": 9,
        "[疑问]": 10,
        "[晕]": 11
//      ,
//      "[调皮]": 12,
//      "[龇牙]": 13,
//      "[微笑]": 14,
//      "[难过]": 15,
//      "[酷]": 16,
//      "[冷汗]": 17,
//      "[抓狂]": 18,
//      "[吐]": 19,
//      "[偷笑]": 20,
//      "[可爱]": 21,
//      "[白眼]": 22,
//      "[傲慢]": 23,
//      "[饿]": 24,
//      "[困]": 25,
//      "[惊恐]": 26,
//      "[流汗]": 27,
//      "[憨笑]": 28,
//      "[大兵]": 29,
//      "[奋斗]": 30,
//      "[咒骂]": 31,
//      "[疑问]": 32,
//      "[嘘]": 33,
//      "[晕]": 34
    };

    //表情对象，用户可以自定义
    var emotions = {
        "0": ["[你好]", "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGoAAABqCAYAAABUIcSXAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQyIDc5LjE2MDkyNCwgMjAxNy8wNy8xMy0wMTowNjozOSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDoyNDFBRERDMjk3OTIxMUU4ODQxNDhEMDRBMUZDRjAzQyIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDoyNDFBRERDMzk3OTIxMUU4ODQxNDhEMDRBMUZDRjAzQyI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjI0MUFEREMwOTc5MjExRTg4NDE0OEQwNEExRkNGMDNDIiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjI0MUFEREMxOTc5MjExRTg4NDE0OEQwNEExRkNGMDNDIi8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+aficNAAAOVVJREFUeNrsfQecVNXZ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"],
        "1": ["[吃惊]", "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGoAAABqCAYAAABUIcSXAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQyIDc5LjE2MDkyNCwgMjAxNy8wNy8xMy0wMTowNjozOSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDoyNUQ4MDNFQjk3OTIxMUU4OUU5OUFEQTI1NEJENjcxNiIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDoyNUQ4MDNFQzk3OTIxMUU4OUU5OUFEQTI1NEJENjcxNiI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjI1RDgwM0U5OTc5MjExRTg5RTk5QURBMjU0QkQ2NzE2IiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjI1RDgwM0VBOTc5MjExRTg5RTk5QURBMjU0QkQ2NzE2Ii8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+sMM/9wAATZpJREFUeNrsfQWcXOX19nPH3Wdl1rPJZmMbT4iVQIwEgkugaAWnLUUqUNr+KYUChVKkQD+kpUUKARJcQtxIQtx2k3XXkR2X+53zzmyEeLD/9/067SW7O3bve855znPkPVeSZRn/ffzvfyj+uwT/FdR/H1/jQ9X3wzVzp363ZyJJ6X+DAch+L6DVTZZyC2ZIOfljJLurUDKYcqBWa8VrEom4HIsEEOxtk3t9XfB5K2V/Tx0Cvu1yqHcHUnIjvR+S3gBo6C0S6+N3D/GSWoVUKIzIzl2QEwlISuVxve/V+u79gvpOBZRKQe7uAGLRMimv+ArlKadfpCivGCiVlEHK9kA2WpBUqZBMJumlKXqLBEmWHUrIRcpYFFLANxs9nZDbWyC3NaXklsZdckvDBrm9ea3c07mKPncjdAZIRjNI2P9vW9S3LyDS8mQCclc7/3YaCeZGxZgpFyoqxkLZfzBiZAnemkqE1q2E2tsFQyIOvVIBDUiw9P84/aeXjCSmUiOmNyJldUBd0B/6Md9T6PX6wZpe32A01V8h1+xGqraqRq7bu0Juql0kt7UvgUJZJ1lsbLVpRfl/gFCpvjML6mjmBZ9FgrlNMWn6DNXoyUB+MXqa6uH9eAFsna0YrFFghNuNYUPLoKJFNRhNZBAqMFONxeLoDQXRG42iw+dDV7gLbTsb0Lw+hPakjFatAfEsD7QjJ8E868ISXbi3RNq784rUrs1IVW7/jAS4kKzvAxJapWS1p4X2v1hgUh89/8Z9VMYHyV0EcfHoqYqKcb9WTJ09UzV+KlLuXLRt/QKxDaswIBbEhCwnzqiogH3UOHqHEgH6r/k4v4YWH60tzWhob0NNVxf2+vyoC0fRqjEgWlgKU3kFLCYSeF0VUpvWIrltw2K5asdrclfbAugNrUJoCuU3IrSv4qO+HUHxhQe8kL3d/aWBw+5VzTh3nnLyDKRI41s2rIH8xUqMkJKYXlyIyeMnQFXQT7ztw6ZGvLRtHQoUZjwwY/pBH9kajCDHqDv2d8ej8NfsQVVtDXa2tGBHjw81KQk9OQXQEczanW5oqnciuXZJhAT3RmrPjn8iEf9EcrjTVkbW//+/oBTkh2IxcvLNSsmd81vlzPPvVk2fK6G4DM2b1yO5ZjHGSAmcWTYA46bNAog08OPJxTvw0PINcPdTobOpG7eWT0BXIoSiPDMuGz8Mv1m4BuurOmHPUeM3s0ZipDvn+M+J/F3drh3YUleDTa3t2JVIoTO/FMZRE+HQaKBcvxyJFR9tSG7+/Bn4ep6TXNkp6PRfi8D+9wmKYY4OuZOIQiJ+jmLKrD+r515SqhwxAe21exH47F2Mivhx3uBBGHfGXNJc/b63JpIpPP3ZTih1CiytaUCoyYdwxIChJSZcN3sI/rZ1N7QR4KaxQ/HHNRvQmvBiwQXnn9x5RsNo3LyBhF6Jz1loKj1CFePhHjQMxsotSHw4vy6x+rOHKGR4UiLrT0Ni6qsJKhxJCyoe/47pOVtRNMJU2Ub0+nHV+Vddrj59LnqJWje//g8MaKnB9f2KMX3u1YDFfsjblQoJN88YgvZAHL95dilunlWOppAabr0Z5dku7F2+Cu9eeLZ4rUYlwakwnvy5koLkj5ssjnP93di+bg1W7F6L1Z9/huryEXDf+oci8xkbn4j/66kfpKq2/xAmy6avqsASr89JCFv1dfsiubud4e585ZyL/qa+4OosuXQQqld+BvO6pbjaZcPFV14FTVHpUa4lTTpWbWtGV4sfU0YWo5SY3+S738E77dswbWga5lbUdODfn+/F8lvO/XrO3eLAkGlz6AAu3LIeH3++Fh/+/UF4z7gIjt8/NSp6362vJb9YPVAin3bSy6PWIFJbD5nIjaTTfgcppD5G11QLyWx7XH3LPfN1P/19ltdkQ/Vzj2HCxqV4+PSpuPzGnx9VSAc+rDol1FY99EYDirOM+MmcwVizvgsWpRG7qjtx+l2v48axwzHE5fraQcFZMQaX/ugm/Pn078H5yXz4CA0odDAiETu5DyT3QsEd4p2diDc1UdCt+g4sqg/q2pqLFWMmv6H+wc9HS0NGEkQtgnP9EtxGMHfGJdcTTulO6GOHFTthzTJh3Z5WTCzOwq3nDsaLm3fghTX1+G33apw1oQB/OGvsNxpRuE6ZimFVNVhYVw2PSq2FUsk4GzxRIUlaLVKRCKK7dqd/V6lOmP6rvirUodcH2dczQ3n299/WXvNTQy8JpOlfz+B7Pc248eyz4R457uQWya7Hczeejv+zdAvsRi2i6jgevmIKbBoTVna14I7h5d9KoGkmJohwCOSfyKFK1hMWFCmypFQgsmMPEYkwFBTDnUyMdvKCYsbi7YYcCV+tuuZnL2jnXYfWpnpE33oS12fbcNFtvwB0xq+0SGcPKxLHjhYfSgwK6AkKEfRhot6K8J5d6Ar2IkksUab/KWhBNCo19KS9ep0OGruDFtf6lQXlJuiVgn6SmFVJ12ylRW7el0A+HsgzGJBo70CCYI9/PtlAWnWylkRWxEnUG9U3/PJJzflXo27Dalg/fB2/mTIJI8684KurclsT9lbvQbvPi65YBMsCYbQEw/DFE4gys6bFokgEKTpk4WxlqGgR1MSo9LSONoIXu1oJj8mIPLOZ/Fw28vqXkfNznJhlm03QthJqGE1Er9V2wdik46PV9HrIFEfGauvSLkIwvm9LUKxNoV6Gg3nqa+98UnPB1di79FOULFmIey/7Plwjx5+0bJJNdfhiy0ZsamnFrkAQDbICAV5YimHUpVlQWWxQGc20ABoKAFOZyoXMOVohsDidW5jovY9ilIaAF/GuDsRbGqCqaYFtZxUKV67EYKsFY4oKUT7mlOOyOJfZAkNDB5I2O8mHvjglHzcFY18UralB0u8nyDN+pbTUiQuKvkz2dvVXXfyjV1hINcs/Q9mSBXjwxlugKyk7qZOoWbkYy3buxAZfADUKDeJFA2CZOg5aqx2qjjaRv5Oa66DcuREaEoAqFIAqEgKXOBSppDinFFl5SqNFnOA2brIgbnMh6cqGYuBQwi8PJGJdu5vrsYGo95vrt6D8i4041ZODGVNOgyKv6MgMkARkJaXoUaigTSZK6E9LRKnkGIvOQkr29iLR2kZkQvOVc4cnKChJxEmK0ZNeVp93BVordyJn2fv40w03nbiQUgls/PAdfLS7EhtiKfhyC2E/ay6yPQWI1dcguvJjqLesRV79buh72mEI+WGKhqFLxqFJxIVSk/GIf1OsPMKq0pbFpY8IkbRekx0+WxZCnmL4i8qQHDAMurGToc6+GDuqdmDdqs/wzosvYG5RPmadfwlgODT1a3Q4YZdktKVS0OYVPyjX7Vks6XS1x0zcZqoEopwjffUE74kJirVXkkYqp8wcG7M5Ib/9Mn45ayb0/U6Mge345F3M37gJa2XSukEjkDt2EvkTNUJbNyD6n2dh/nwphjXuQbkiQYyPIIMIgmwmp2wma6HFSSCdolKrlOgOx4TA9EqCPi4skvYb6Dx1tDjxRAjdVZth37MefllCp8WF9pIh6KiYAOO40+G86mZBgB7+6C0sffgBXDt9OoonnvalgM4Op0pBltGI2MzzXfK2DSv0ixeWpDxF8WMJSub4S/g06VsWFH2xZDCVEFWFr60FQ4w6lI2bcNxvD5IWv7DwLXwcSSFeMQ75o8YT7hOO79yMFBER19L3kN/RjKGePPQfPhKyVo8I+wRakCQtPh8JsqY4BZ7RWFR8ps8fRZZeDQ3RaBW9NEkLE08mOBcFbzSJVm0SFqL6TlosQ28QxZuXILBhEfZ88G+0TTwDmukXofDHP8WmDZ/j1vffxHWVu3HG1dcfsOAKODVq+JqbMba0BK0/vi2vfe/Od43NdbPkbI9Yk8PKiYgDEwmR09Nqv7KgTiwzwV8ej3aCaHFazCRnOpHjeax649+4+aV/4u2c/nBe/AMUjZmAWEsTks8+CPNt89D/n49hNAHX+HFT0L9iFGJaA4IZC0kJoqkkYehgMlpgs7pQTFAZhoYEKcHjdIkFTcppDdbQax1EFBJQCaVn2h4igbdLGvS6PHAXliCPYrFxbzyJst9eicijv4Ob/JT+lnvwYEcAf3/of0TCdh9F1+vQGQxivE6Fawfko/uyG2emNLpbEfAd1VqEoL6mutYJC4oE0ykHyZnr9AhRDJMIHSP+I8f/3CN/xO8qa+GfMw9lU6YJRxv5eAEsv7oKuY/fi7JoL0pGjEcu+TmX1YQwXWCULEcQF3GkRK9EkiwlzhYVj0FHUFnd2QWz3oD8rHy4XbkUJGfBTAKS6H8qej5M56clXLQbTDCQdcaSaaao0RngNdjQafNgTCqG6f/6I0y3XYLUF2tQcOMdeNlZhEcevJeYbVoh7Ryb0aXvjcYxjH6fNfNMtMy84BGFr6f4aPAnRyJfWwFScYJcgjuAmkmTwlwa54WIcNR+JJdG1Ph3D9yLl8w5yDvv+8jK8SDY3QXlY79Fzm9/BE/lRtiLiHq78+C0WJFvMyNBVpTgBpYjkk4ZJvruBvqcuq4u9HM5EaBzUJJFGYkMOG1ueLLzYSAi4Q9HSEgG8T41OX+J4h8VWYCSFM6sYyvpxdaEGoXlo3BO3RcY8uvLkPjn35B/yVV4p7gCT/3lIfHe4oIimMgftkRjggRdb1bCOfcSBIaMeh0dLWkF/jLyJBJI0euPt5TxNSdlBZPxyn5vCy9MhJQlHIke9pWxxlrc/tjDWD50IsqnzoRab0SwcgfMv7wKBa88AxsFoWqCLx1nL5IxaJIhYQnpYFI+anbdoFVjeVU1tCoNCh12RAh+GSLZf8XIf5nJYvyxOPxk7QPzS+By5hICmAhCFbAajLBaHDDQ+YAss6WpFqvaeqAsHYkZZhVOefRWJB+6G55z5+F1RwGWvf0acjweuExmBNhCCO5MdB6XDxyA7unnj5E02ovBfz+cf6K/SwrFdyGoTOrI721QxKOIEhHoPYxFyaRldz35GLaMn4WykWOQUJP1rVoE5+2XIm/rWigL8kTgquULIoHnUPSfiFKA21ZPepAkn3J458s03EYWUt3ZgY0NjRiRT5+jkJA6iO8kSQh67O3ogJIEnsWZBY2e4iwNfZdS+DaL2Y6etlaUTZyCyWefh0YKEdY0tSGWNwhT+pdi2st/hvzAnXDP+yEerW1B1TuvY0xBPrr8PiTCaaHMoVMceup0dI/53pPoajvIqtiKOAnLB74bi2ICoeb0UbUyGkGEBRUKHVI1ffCZJ7Fh+GQMGDYccQpCYx+9Cfdd18AT7ELEU0iLqyIaQDRb5nKGBlayEF7EMAWxLe2NCJFv0Kg1dO37rYshT09+J0F+6sNtO2Aj3zTIk40Aw8sBX6+hcyLOh13NTSh2uqHlriUSfhstMgvaSLAZJ1jyebthtlpx8WMvYMjYEajZvRvrSFhxZyFGlw3A5P88DsVT9wMX/gDP7K5GZ101lERmQuH9JOPCHBdCk2a5YHfdhAMVlgVFDJMZH74riyJT52bJDZLfizgRisCXzH7hq//E+9ZcDBh9CuK02LEP5sN57w0Y7jDBVNIfyQTFOGQFLCQ1szOdRohCFhVbrSAMbZ3N6KQgl0kEN8cy3CnoPSZy6mtra7CNFnRscRH5Hz2RjsS+707Qzy6bHdVdnejwejEoN5dgMUbrpoA/EhOC1lBMFGP/QUrQUVNNi2rAFX95GllOE6pqdmNzayfgyMHwwgIMe+4P0C55D83zbkBNxSlQptgn7ydPkzVABYUn3uGn/F40kIrgliycriHV23tQbo+vgZUtTH/vaevRtTX0mLuae4xBn5den9xXMP2ayIQiTSgCvmncVZQi5+0/QFCN61fh2eoGFJ4+GzIJNLr0I7gfuBX9rQYUDRxCsU8KykxGIU502arTwkAMMJ5MgxfnF5RkEXx4/T1o7WhCoNcnnnMQc6vsaMXi3VUosDsxvMCDUOxga2LBq0g5OB2Vb3Mg32ETr0nQAvnJl7pMRloQBcIkvAhnN3hR2ythHzQe5975Gyh7Y9jR3Ig6XwQmTwFGu2zIfuh2gIJd89AxCJHFh6LRfTDMizHDZUfvqElOhcU+D9y1S+cgh8IifaQg9FGQkrBFN9V3j2tq9D6s0WlXDBhVUTvxrGlNQyePbbC63J93NPufrq/vnhmLhIVSfXVB0RcqNNrRicEjz0uQU02x0+4TFEHhc4sXIznuVJhokSI7NsHxp9uQo4wju/8gGJRa+Ill6Yg9JTmvSdKyUCApH4EwMPSxdbW0NyEeDqCNvm8x+RJ/KIFTSouF7wlG91dcmbZnO1yookC8imBvbEmJEL2WhN4eCArKX+hwUNAsI0g/R2hRlULj6eilmOrHN2Hs3DPgb2zFtrYOiuEUsBcWY1zUC9UTvwcvYoKo/4HQx48pWgnFI8ciNLDiDrmnM50x7w0JIqEitOhs6urX1ORfMGnO9LW//9eLtz2xesukJ1Zvzr7vnU/Njy753P7k2m1j73/37evOuXreR72+yLKG2q4JSqV0iIUdv6DYdMm8E6WDHtBOmwstOVBZo6HoP61hW1csxkpZhUKCvBAtlu7hX6Iw2AFDngcFVjuC8RSCpNXclszWpOP2ZDqh5NHiEHrKbjQJn/Pprh2o7wqgn9OJkQW5iCbT1tfnvwzke2Q6xw83b0Q/dw7Kst3ooSDVSu/f29HFoS9yLGZBPAK06JFoCjp6jkvkCb9PCHXu7b9Gbq4N9c112N3pg46YoaekFOXrFiHx/uuIZHkOEhSfuYGEPY5YYc/A4aNocQfyuSQJzpSSjLqazjNNDtvOe199+ez73/sE075/FbKLSg7OULncOOXMc3DnC6/gsWWrpgwdP2JVVW33LQz7Bwrr+AXFJeREop938Ojp00eNRjmtUZhMPygawaN4v2oP1CNOET4o9cKj6LdzA3JL+8NBjM9tsqA54BcXxiyN/9WpGLOTR85WkfWyTzETQ1vf3EJxk58Eq8HQHCt6/Z1o7myj74pDTRSd4cxNyrC+Zg9qSShTywamT5kUIkUXy/FWMQlYEpCbQjfR9iT5eQstEoxGIbxYQx2cFVMw9eofA94w9pCf6yTrNRHhGORywPb2C+glBQzpDQfFdPwYo5GgLadQuKDk+6lusioK8hvawtMK+uW9+8z6TZrTLrn0sPFgr68biQMyO4PGT8BTazZi1nln/rW6wXuLdADeHG9lhczZj3h+yU8tw8fi3PxcEoCaIDkCtc2J7tVLsI38T9b476H303dQ+v6/Mah/oRCKQWsgdCH48fthJL+VoMXjv+vUSkEEGOulwwmJ8N1qsWMd+Yy97Z3kayQMdFvQz25CF+G/l4TV1tUi/JiGPq+bFuf9LVswuWwwynKzaK3DFERbUNXRSZYcwUgiH1FSIu5V7wwECBIBV1ExkQk9CTSdppKDrZhyzY8xYORAtJLg9nYHoCamZyVSUla3E8nF7yGgNx2yOiP1GgypGInekRN+jPY2tDd06a02w4dPrlgDd/7hSyjdra24kKD1veeePuS5e998F+MmjPxrfX3PKKVKcfyCkjkODfikngFDr64oK4eOywvco0aCSpLz/OyLDQgVDUC0vRX5bz+HUUaCNw5waVEctNhBWqBugiGzViOyDhqCPR0nYzkdRH5IcQCF5TjISP7JbLbh86Ym7GV/EeGSuBajPU6RBpKIsbElcVopGPLRQsfx0fbtUNCJjsx1oov8WZx8kJG0n+OtbKMeajku/B5nIzqJmlvsBuQNHEQXF93nF2MUn2myB2DK5ddAS1Bd19OF9mAcJgqS+9mtMK9ZhI7tm/e9vk/BOjg5TAG2OhrWJ4gV9kRTD/7y+edU9tz8I+J61ab1qPcHUDx46GGf/5/578BoUL7S6+09WFAqwsQjHWqCNpXReFZi0AjLGEu6D8JJgaeRzrSuoR6r91ZDO3YKVO+9goqqdVDn5tNFKGEibcwmi+ugmILZkoEJAufaSFBKKY3xTJ8ZQfiyWUgWYoJavRlrGhtR29mJ3gizOQWmFGWJZGs4nti3QEw2irPzsbvHi831jTi1pACRkBeV9XtIaDFUEeQ1drSj2KxBKzl6Tuo2eXvg7+5GTr8BKKwYDrmn54D4hxiarwFjLrgE5eOGo4WsqjEQElZlIf/WT0qgfsWifdk0FtZu+venW/ag7S+/bdJ/uqCiudHrnjRh+M2Tz5vX97LDPtZ98A4G5Rgx/HunHfZ5R24eLrrlxrKWnujMgwRlSsYOexjpsHrbkfIUXZ49eARGqNJKkmO1wEJA30kEocPqRJIEUbp0AUqsNiRUOuFfHORfTESrWyimYWloiPGxJaoV6fNXSBzTxIkYxIRrtrFjp/euJOE39XSjNwxR+T6tXw5cBi0Ft/GMg5UEy8siv9QVT+LD7bswOjcbBTayYvpdQxZqt7mweFcVcnQqovZa2MwOgscQEZIOgjgZZRMmQ+MpRayvEpBZ/hgTAUchxp4/jxRURgsRDR8TDyIrA1xO2Kp3kHalg9t1dNy5ehMCD/1qi2P9suERf6i+Nyr/cMYPrzsmSm1c/CnGzjrzqK+ZfsWPYNPiyoPqUe548EjeiWhSp7q6sP+ckYTp9swiZ9sdMNWTQ3dmQUkWpFn7GSpaa2DI70eaqRYwo9fqhQVxVoBpMrcrqxT7rakPQoIEF076HD9R4g31dfR7hGg4pxVlTCvNQZ7FAB8FrH0kiOtRnHtLkaa/9cUWFBkNGOlxoTeWEHWq8oISfEGBa2t7C+aWFyOakigQdmFNXTXaycKc2WaMnnMWRcghyF9We27cCbRi9FnnYOUrL2LPnlq0EREZQErg1GtB5gg0E4pAg9s3bIVl/gtvuZtqLgxpjKloYyVceowaNH7S0ety5K/b6mvxwz/++aivKx5SgQEV5WMOEpQ1ETnsi7Wk7d0a7TRfcbmp3Lyf8WQ5XXAkomjOLkSQrGX40ncwMicfQYrqdRnqyrk2P2lfD2mtVqXIZB/SVsVwx0RCQW7cSlbXEIwSJe6khY4RWUj7sRkDcpBj1gshHRgvZZktkImkvLZxGywk+FP75VEAm0SYfKaHFKiLtGPxtq2YkJ8jaLKF/B03vHAwG24PYvLV85BzylTEm2oOmxGIkzVrCssxfNZcbFv3MPmpIAopaLca9OhW67HpgV9hPvm+cVoLhrRV3d5uc6T21DejI5hAdmGWy5nrOaoANi9bBA2hx5gZs49peTn9yjwHQZ8kunkOPVTRMAKuvDkqIgsDlPt9oyqL4CiVQMSZzb19GLh3O7QOt4AyXswEab2W4M9H7CtAFsLlBWZ5OrI0FQkhQcxOTWtkNVrQGpOxg9hSOBIndpiAnVjUnLI8ZJv08Ebi+zIB7JNYEBGVFi+TNhOfxKwBBYJyB8mSGDqVBgve2rgZhfTeUqeFfFoS2XSOmxvr0NxYD3euFaf/6AZycFHhEw8b1wseH8DIWXNQWOJEI1kml/xdJhMRGQm7dm/HsJ4mXJgKoDTH8/2cQBspehQh9r96o6Tips2jPD7/4D0UDxoKre7Y3cManU59XKwvGQ6hO6dwusuTjwLlAdqn1iKHWFyA2E5WawOKyYkHOBmZ2UfEFqMiuOsibYwSYVDTezmdw0lTJgVc0NMT1a0JRERurjecREcgjv5OE+YMzCMioiJLiqdLYCQgNS1egTsbjeEE/rVuM4UHSsweWCTSQyE6B4tWSwGsFQu27YSSrGdScR66iYYXEtloDoaxsboSie44Zl1/M1zDJyPW3HDEEgRbWaKjDa4Ro1A+eSraGjvQycpGMaGCFFBJPjnL6YaPUCihNZyV0hqJAGnIj0kIdLV3h/y+o4SjMpbNfxUjp808vvYFHzv4YwiKwSqWSOb1ZOUP8jiccKqUB+S5mFDYEGqqQ25rLbKJwsYlpegC6qPb/KpOYnzMCFg4nAxl/5JjtkLSGLHLG0A9MbbuAPdCAKM8ZgzLMojXhRMishE1JqfRBJfDRSSjDfO/2IpBdgtmkiXFyCIiZDFmjRpJitXeq9wDHwlnxoBCRAgJTPQ+jcmOpWQB7VVtGDvnVJz6k9uR7GnEkUuTfQlesjaFAYNPPR0GMpCO3gB6EzJMpCCcdE0S6ekhJSbiMtpltjrsejUMJgMqW4LLty5fetR62vUPP44ZFAIc6xEnxa7fsXXnMZtbOFscUqon9DpzSRCHmmlOVhZUy9fAStoXI1/Q3t0GBcU3KoqhtEzF6f0cnHJ1lYmEMpmG2HZibzu7vWj2BpFMKJBjVGOgy0BWpISfnkuFOdOtEaUMO0FdJ/3t7c270N7jw3TyR2Uuq7Bk1gSzVi20fRX5n0gkgdkUaCvkBBKkNIV5/bGoajd2bNqJ/uX5uOwvfxOpsDgxO9Gof9T8Mwky0IGB4ycQTA1AU0sTvG4XzDoNOojp6EnZUsR6SXWVJoOlorptz5I9bcGB5Z7si5yevCN+bkPlLsw8hpDihAhqchvbVizB3sqWT45tUeRHYjrD6LDVBY9GdVDaRAgqvxAWcshobSJti4tMdyDoE9kCH7GjEPmNABEBJhCcPWAm2B5NYDU53vrOIAwKNYa5DRiVaxZkIxBNilIF5KQoEOrJf61t7iSo20pxaRQXDyslimwhuhwTFJzjq71ePxbVNiNJfm52/wJopST5GBXK+g3G6roarFy9Dp4sE6598WUY80oRpdjoWELat2AEYXqKt0rHngJve4D8FBEustxYLCxylCpitw4KxJvrmwuX1PhPve4nN+16s6l1YsURYqMAkZSzKcieazvybo4OIlR9+c93nvoLZ0yeP7ZFEUyFTPZBUaLc+gw76qurCOZHQZmrrRHJtibI+S6K/rWi3J0k7A6F/AgSRHDfgoW0nilBIzG7rlAMZlrgoW4bBmfbyH+FKb4Ji14JHS1grs0BFdH6pkAUn1fuho8gZ0JBDipynaJvLxJPQU9+rocEV0kWVtnph8doxKQCN6QUaaLOgrzcQqzYW4lPlq4VQrr5lbeQVTEB0XryX+TLhLPvq7xyjwZBzOGIBSMCBRokqAkw/OslEYd5DDZhsZyFz7WasbumBq3Owj/f9/yvXGddfJHcvqdKWrN4EQx6dWT65T/kj9HtJwZ6XHrNpTCTGznEd9HR3Noq1tZN57fpsw/xwfwP7ivIMbYdU1BJgix9PFLibKnDf7whGIjZzLKbBb0Wbyat5gZ8maunkkvwRNYG7hBS0ULs7WgVFtIrK0hTQuSfUgShagx2GeEiuONKK4W/BHGSgEpaQbSH49ha14TWnh4UWPQY3z8d7EbiUdGc0kuw0EwEZUeHD5FYEhVuJ32eiUhMAk6XByaTDR9v24SlK7Zj8LB8XPfiK8gaNh7obYe2sBjo7kJbUxP85MtY30wmI7LcbmjdbrFcKa9P+AaxjZOvM9iNkuEj4CkpQgcFyyGbBVZiaxJZ/Q66pkWVDTj1immuLLMKj998dddH77zzck9XdBHZ3tynDcYfTTl/HuuoYG5aYqV3Pv/ywb6QFCRA7qGXromFVejxoKO+Gnedd94ms05xtyrDDI8qqJBGry5IxDzOF36PD2t24jfn/gCbS/Jxk10PY4aCevIKRUMJXzRHRZwRZwGGCB56CaJYeJwtsGlVcFg05IwJshQyggSBFvJ7doI3fzyB6m4fdhNF55jLY9RharEL2UaNIC49RPHDKfo8QsU6X4gof5xew6V4C1w6SbCxHLcHAYLf1yhG2bGtHVMvOBVXPPo3CsAH0Wp0Y9PqNVi5ciW6MikjDSkT16PURES0pOl2mw15+eT/ysoo+HYSKiQEPCUoOLX2K0X+4GFY9e67CFLQ7yQf3EJIwZmSSfl2TDakUDZmGCbNnuYKOWx5az9dtFCqbV549wWX5v3y+cDs2df8eJ+wDnEvpAw6snK7Nb1hYe/GDbhzzswdvf7I6QUUGiQSqWMLiszQFdMb7UWxEC5a8BTWVG/D6xffgm1jTsFPSYNGO22YPHU61v/nWZFcTQtKFolXrqDGUtxmLKPEqqWYSEMsjdZMJuvRmujkNGgJRbGhvQktvoCAoCKCkvHZubDR4nOWvYeEGSAhd5Bg2kPcz5eEnWB0mNOIfJMaTjMJ2pZFkYIeO1oa8cHiVZBoOa65/07M/PkvaGkcaNu5Fi//+2XymX4UlxRj3MQJKCoogJWQgNlpmJSAhdfW1oYgaXZNdTXHLrBZLIjSc1xKUelsKBw2HMveJEERiXEYiBxJCQwmhbU6c9CvX3/otToojbk4a/qMCz7fuGmM225bL6/aOOf3P7j2L5uXLb3l+7/6DQoy5ZeDO8sUYj9XlCxq/uOP4MXf3fMGgcdVBf1coUQ8eXwtzWT4FjmVUvboTNDn6zF910rkP7oHn1z8U/xk7Km4w78Cc0r7w1sxAuHuFqS05rQWivbjlBASszwu/PlIpyL0e5hob1fML2AvQn6GLayCFr6/yyEYVXswhD0EP53kz3wkWXJJMBCM5lLcZzFrYFGnKEjWwWp1w2SwoiMSxbIVS1G5148xZ4zHBb++B4UTZ4GrZbtWfYR3F7yNssFDcPb55xJFJN8QpLCEBMB9CqxUZhKYhaypZNCg9EArCluCtGjst5RSpsNJjqJwaAWsDh353BBk+l7uyCX+QuQoibqGRmz950uYPPsMjBgzmnyXdVowkVjvLCuEvrHpZ2+/+O+FqxbMv3zCWedOHjVtWlFe/zKNhgTLoUdXcxO2LF/mW/P+wlWVlQ1/z7Iq38ry2HGgkI6n91zLFyPRwQVCFTnpIcTmPP+4F4u7mvAwxQudxLnHlRWhzd+VZivc1UoH/8yVXMb5Ol+UNCcqhMY0k2Ewx6SD3mQWIwiiyRTWt3YR5U6ImpFIEhOxKCKL0RCLYyZn0ipg5ySv0S52l3NaaeXaVdi2qxMlIwtx09N3Y+KVPyBMMxJ1qkVzezt2bt2CK6++GlmDhiJJFhcnBThiEwlXbumctVlu1G/ajBCRmNFzZiNOcR738uWRNeSWlKKntQ0Jh1XU1OIEfzqTB+6sbIwpKsLWzZtRXFqK3JzsQVt3V8JG/kbV2YV+FvVnoVDos3dferXk/ZdeHUAhVzG3U5A+q0jP6qJApd2A3aVFDuFCEonkCW8SSCUPaJJlptNjdpDzj+KCFW8j4k3CqyQraadv5QQsQZUms+OEhRQhfOXUkUWr2IfHHJ5w3wRrajfFPdFMY4uehJFLsZjVooOFBGjgcgeX6wkaFNwCRlQ+TGSi0efHlm2rUNscQtGIElzz0A2YfOnl0Ob0h9xdj2igGeyAuQHyjNmzoecGzbpq0XQiZSaJMbRJZEkQjpr+FiGII0tK8sYDbRbWL/4MvX4S1PlXQ20kCKfv1OUVIG/gYNRt34EYEQuNKt2WltDoRJA9aOhQ1K1di2BbK9w5ua7uL7ZA378/YsUhpCorKRg2otBorEml5BrRpSTqarQ2dG3cAMNMgpPQJ7Wbg9ebLaOvCitnckNRnZF+JrYWaEDpkIFEvYmuku8RdFbJGXI1wV8IITJf/r2QFp8DXoZC9j2cZsomtTJSgKshtsZetkutR7PKgE56r6TUCLaYJB/mItJglVVo3b0LtZU1SNLaDpo4CbN+fQ5GzTkbOs8AYnStJIztIiXEAkkQpObk5EDJUEZERUsxWSwchJaoPyy5ZCEtaN62Dd1kdbw4rpxseCoqoNS40PjFEvz9gadw73OPiDX49JVXMXT0SOQUDEb+oMGIxWWhXHpu/KRFKR81BiZoxYYJTplxW5rbbPKUBttR0KODyqFGg9OMRm+YoD1dQZDIkSpVX+LmX2XbDWlgnHNpIrhTpIkCJ0YNJhP5EYIzYitOWvAIxRdxpY4ElRBj2ljbdEQYZEUEcYKN5kg3jA63YFq8KyNFnxUkQ+IqapfOimqVHq1h+rxwDJZkEJpIiKy1GwHS8mp6jcZmRlH5IMw+ay6GTDkV/cZPBIxEp30tFBvtSFPpAztS2XLIaglusHfXLmKmeXAOmUDkrxZvPfBbLHp9PnZ/sQucRGOrsNFx+tmnod+osXjp3gcxe945OPUHt+LZ267Duo8+wqmrV3GJE56ycmjNFCqQteoISfTubDgKS5CoqiZHSsoQiaCHBNVPLY8cFmy/1OHXvMKsMq/Iji2klFUdQRHY68kvp05w78AxLYoElYrJskKT2dEndnpTRN4a6IHbZICOqHaC4i3WMg4alfRK0YYlqWihezGkYggxMzNqt3xBGhxAPJpO0HYTJV4sqxHQ9MAQDSEnHgL3CCn1BqhtdngGDEA202KK5AuHDoOH6DFc+cKxJ4nGx7s600nVwyRWlSS0JJ39iuXLkVtcjLb6erz+/CvY8Ml7eHNrNcbogTN/dBlyikthIiLB2ewPF36GuoWLcYrLgBETRuOnp07A28vW4NVXnoXa7CE5tSG7uAScHgoRO9TTyrnLh8LszkHPzkpSSgXcROsZ4hRcGddoX/Yq1GvVKUW1hiB9bL6V2KIam5r88IYTwrpOZIDqsXxUgoSViKVSmlRGY3WkIUR00BOKYGi2A+FoGFoN08uIOEEBk6TREYKcziYvyq+fjDPvfgCtqz9Gd1Mjwr4eYjrN2FRTh8HDR6GIdFpHcZhEsKQnAZkcPAgkG7acXEgcwUsaUdlCTzPg5yFYCcTJpxxrl0ScFrOCAlWn20W0+i386ZHHwdW0px68D9mFRVj36SJ0tXegfOwknHvTz9Gwawde+N3d+PC1t/DQrfegrKIcqzctg2f4FNQsfw928nW20jISbj+0rV8Lm92ClIFQgxRUQ/40RVY2ZvQoyCYLVhGByDKY0Q75IxLEAO7z4JCl1GGAgxBoe1svGggKOQXG1iXLX11QMlFUOUpfQsISvXFmnVlkD9jPWDQSfMEAWUy2iKPMaqXwQUJQxFy4sVbOjKXJGTMeOQxZCivBYQuGk9PN7j8wk25MZeiK/kAdyeibGo1rP0FXd48IaD1ZWSgkZpWMRI6yMZJ8IwXkOWQB/j1V+M2Nv0CRUY1nlizHQDqPJ35yLT58/gX46Guf+evTuO1XP8UP//gX3PPqm4QGF+KDV+bjqttvQWdnDx45fSJyPVm44bHHSEutyCbmV7d8MZJmA1RmK2Rii7yjkDdzy71BUiIKhLs7xa6RuEbXvyMavl+vVP2KheEj8mQmxjux2IYG8l/b23vRTfGhgYTFxEn+CoKSRG6WsxQkGN64QgEAWjq9ggjolAx3stjiwnGRmSh3XOTMJNEtxGRGLTIYEcQIrpT0864NH5G2ViC7II8IwF4hDF1BIX2LFs2bV2D76tWo3r4dvT4fcosKEY4TCQn24Ipbb8WGDV/ASwwrL9OrfaTdfOI5sdHZgPdeegnc4fD6ho3E2oagZtsW3PzXZ5EzoARP3P5ruMi5//X+xzDh7Asx+JTJuOvlN1D1RSF+cflNKCjJRiExtyvv/jUMRrbHONwkfIZcjgF1brcgQnGzRbQvy3TBUb8Pka4O4aezjWZWrl+Skv+d2Gs1r2YwlhSEosihh5OUZ29XGPVkXV4SIjekcmWbu4i/LLVjCYrPQ80UO0KLw44wStffTpqTYyQt4iotxzR00jJZlIZ8FQuKRRUT/orJ0P6vULrzsHHZo1j+7vu4/okXoStyiFNo3bEaz/72PrzzxntiLKk7oyHdnKikYzh/XiiMnzz1BDEQE0J7d4vk6lEro+Tc4531eOuZp/FrgjsWEreQ1e/YhqbqBix7bwGcpNn5uW6EllZh2fw3haD4cfmvfo1n77oHM79/Oc669AI4B5aTUtWSQhUhu6gYKrKWLoJ6o9Ml6mwGsnA5TvBtNMJbvRehzg6xhZWvIZeEVef3/oOWbYrUt1meFMxHforXc7iHiJJdT2FHGG2BGHqjHEseWi07lqAMBzZFqmmRugj2uKHR6XKIVi+b3kyBXxJG3jme4ty5JChvNJ4WlFKd3jqjEmOsNZh99TW4evQY7Nm5B1f84ucI9XTh1/OuxR569qqzz8CMy6+APTcfBQP6i/69dR8sxIJn/o7Hnn+dNFaNnz77IrRWgk+GnKPsgGAfxos26dyLcM4tt6W1jtBgyX9exRvz38GQIdlIJI0oHZqLwlwfjOb9S5FVPID8a4fYQ9Xe3AIbxVDcU060Dhbynxw7cd97fl4++WctFq5ajVwKpsfOvBDemj3oaKhF7rDRpMiEMiQUp94wuTMcnKxTqlbIBxBTRh0mYTpau6HZZvRzJBEgQYXjqfRsyhMQlA0H9NCZCYf3tAdFO7KRKWacc3dKElgIDsLeVAYs+WS4j4ELwkphUUbsXLcObY1v4fTr7sB/9u7BT2aciZ/PuQAtZH4eWoMPlyzFkInfwyt/+gNevO8h5Obn4tr7/4gZV/5YHGs/+Ri3nzkLWkUS1z/9EpKNVUfF9HhvAGpy7LOvvQXaA2bjzfnRDYj2fExEwIqODvKzRtLuoArj5+xvNPn3Qw9g+PiB+Nmz/6IL70GkoSFNXsjpsjWR+cBElD9AgfDaz9chQtDdv6xMQHzdxvUUG8bhp7itgEISb8ALBwkzEIs+mpBTY5WS4hDf0icwTlnZiGw4jYdC37F6JvL3mRaPcSOhtPl7iZbrRHHPqNUjzBkIYn6pVFzEBnwiTCgSGR+lEpFdCjlDhuJ1sowryvvhs4XvYcYFZ8FkNcJB1//cps1CSKLxkALVLZs3YfWnH2BuxUgsee018ffxM2bincZWbPliK3Z98jY0TufRUyqpNBsrJWZ54GPsGbNx7s8ehlXbjf75XjTs3IR+Y3+IsjHpYt/L9/8Pemp24dr77ke0rQpxCopFoZFVnC6op3oPQs1emImaV9bWYcCwobj46qtgJyus374JO5cuQiFZWoisr4UYrt1ih56QwK7RjSHEGX00MsCQyLAXIj/GyYK+43gsapDYUUNsK9/upKg7IXZCFDqsomHFYbCIWQ5KmShzSoVEJEh/MxJDlIWWsEUpRJ9FCM6iIjy08G3cc+mVePTan8JdYEdndxA//OVPUDS4Ars+Xy1irNrtu+GwKVAyfCC0G3fin7+7HePmzIWBWJaNYOeu/7yPtk0rIMcix9iip4SStwn5GhHT2okGqKDnGIeeGjf3ZpSOmom9m1ZDY8yhn0/HtjVrsHnRe4j72nHva6+KGC7W1ZXuSBIb6eg6Olux44MF8PmS0BGszZw+jTDHSpdHdIV80bv/eB6azhaoygZBRSRrXVMdTiVfarPY0Evnq1Oq70zIyUu+bFVfRxw1Kd3MkiKWp8PuNr/g/VyVT8ZlCvIY9sIwCGpJRIMrwjFaII1ZpJP278lSwbttA6ykaY8sXw+E2zH/0cfw2F1/xKQLLkv3EuzehZuv/AFOGWBG6bD+qK7swIgp5cgh3G7ZuwOlI0QfIvKKi+DSJRHr7hadUEffb6xCPBQA7+XyBiKo6fYhRCRAUmlgcefDUTiR/EgUWz99HUFa4BGjh2DIrNtFrBZra9sXNIgroAUP1jVgx5pV6D/Qjj0fvovdM2dj4LwbBOT9/ZE/oPWd1zAqz0OKCgpVKJihoH9VbQ1mUtDuJmF5Y9ELu2Jxs1KpCHydgmIiMZUpsEWnF107rf6AGHLIe3d4yEYkKSFKmuLWagRVZk2JccNHMtHXyp3Zh6uC0unGq399HCs/WUFBbyv2bq9H/8FFKCV448fQyaejH8H/pFklSKqc8HWH4HJRgOgkiLXu30GRCnlpXfyiGnw8W4VSklKktXLdWli0EropyOW9wqmuKrJ2laDdo8cOgzZnBjmLOKKNjfu8/YFkhZO4DTu2o5OsvnjAYNiqq7Bo4Vtosufii3fno5PgeKzbgSgpjzGzicKp06CmJ4DdRNdHZntgUncrfInYheQhXpC+LkGRUpxHH6aN0aJzW1iAYK+XYC/PZhWMjtNHPGciFo+KbHQ0E+iywJiic9O/SFan3SXFWCZMv+RiRHpDiJEAy4fVY93q9ehrVswtKcINDz6Mpf++HSO/NxJnX1CMevIfhvKLkVOSnrXUsH0LlOEOuD2ezBjyY4f0fdifIialI3JRxNmOvpCBt9vEEyIBHG3r2L97DnL6owXspWEhSeQky5OLOTf8BKsXvIn+FHhv2bINe264lATCu0hyKfDVi8owFwMlsfFbgomUvLKtHQW0hjkWBzoj4QvDqdQL6hPchK040pgAsoMb+UmeLaQjLWkPhMSeJh1dY5S3ypBGh/mkiFTIXwo2GfZ4P25aIdMAEiEa7qaLu+bhv2HulVcgpTaiqa4df731ln3vPfvG23DOz15Ab4jYUpcGFdN/jtk3PEZB8np89vIL8DbshD07W9D2E77NEBcB6byikSiiFAeKg2Izbs3aFzhz+otnARIj5IN76NNbTSjoJjRxlA+E2ZWF+pq9YitSPyI+LpsTnuxceqlWZGeUYuBIuhGV40/OOPAOlDqi7ya9gfyU8nRaG/1Xhr6MPpXZVKqJYoIKaQlbRYvXL/Y3Cb/DVVweN0CwZ8jUlnBQ3SolLj4dU6UbKcVwKo59WndDojjs+vv/gImnTcL9t/6CFi2AqZdcQQHnKfjeRVejZMwZqNu2BTqCpfUfvEVCbsHAisHoVzECiWBI7NKTvoaJXQdfeEoQEMloS8/K5bPnPVi9XWk/JYAhhsb6BuHnklw7M+jQ6NWQFUnkLiVRAtGqVCIpza3dKim9sVxPQuTtPgMcXMsz6rzx+BT6tI+/kqC4pGFQKO7irTG8tTLPbCXGkhQ95HanUSiYlvwT79Jg2OPJW6kvpXL65ueJaoO46P2LmiI/kEMMkKP4yXPPwiMEY58vWoS6tYvQtmsDjBYLbBTxWzV0sb5mlJXmIa9iFplxjOIfb3qY7tctpD4V4+sgNismSjNScJ5STk8DkFPpgVg+sowUKUqYntNwKV9vpBgpiGxzWs3VXAjkrQ+sqBlBMcz5iBl2kzVb6PXJno4pSoX2Y+lkBNU360GSZadDq7uSnWGQfJJNZ0BrT5AWh9vAJBHI8lANLswGyKJyyD+JMQBfElZ6bJKMoBgYotwHIZwni3HKiQTPOxrKyYrKTztN1I9ijQ3CWvTcjG+2ZG6+EkC0vSMtdcU3eKe/TPU3FfJBioXTwxR4cJbIdioyPyfR1dGBWCSJIPfT05FrMaOmnYukCREzaoQSp9sR0n6aGxBSsiQppI4gby+yk6UpRsuZKsMJ+yhV2vPzrvVf8tZMDmZ55wVDXHcwLEyad2EwUWBT5jamCGmVL5FuRlR96Uu5zKzXqbF500bB1NRfys2xwOIksBidPA9ujzY1E9SoxcykGJGVaHcPovx33nX/jVjQEYTFyxCP8Li7zPemF1viZphwhEKptvSIPiZNpFQOo140nfaEomKPsvBPmZ6RvgmdtKy7NSrV610E8TxSiFj0wOQJTh1THAhXJHutWaW5SSnGcEJs6yS6gDZypDatWuSkolxCIKGJsgfnq+h9nXTCkYzzVIm+iPT+VofDjs2rVmHz+vVQuN2HzXaLjc7sx1gLeT6f6KdL4bt7SGlLVigP2oFBAA+vz4e2xiZSKAlGsh5ZJKpV6bazBPfBp0MXWbQv7I/CuACrUiju8UfCvZwIILgsSKaSzpMSVCLN9OYZNRq9LKabRMhZmsG1KF8oSDGACm7yRyqhYUqRgBUMR0p3x3aTsHpokUN08qKJhXcWGgwINndi6fvviwKg8kiWIabtG8lvOSCZbOkY6TsV1kGmLyoDvDu9paUNrQTPOmJvem4pYD9GLsFAlpSUFenKdiZBIPdtP8ok82lNdsUSyc+5XY4sT03P5540PSfqeLWeFslHGB2mhVcSPeWhUEwqRPBKC5pFzI9HC0REvJDeOdgnsDAJqYOnoohNaunUi8Oiw/J3FqKntgpqh+NQq2KBsANmSkwXzAJT6C0HxDTfsZxYINEIb5VEbXUtOtuCIofIdSMxeo4sxES/c1K1r7qdFOMQ5AO3KHWmhSZv45F16fwnTCclKImHpKjUU9mCusMhUc1lK+OhVJxxsJPjlxTpOj+PHmArYq2SMq9LY7K0L8Dsa80yUqBXv6MRn7FVGRxC2IdADQk2FQ2Km1PyjUrkvjGh35ZvOoagZB5URcLasXWbMBGVuG5x51Kxi5JnOslyulgqZUpCfRFkSnAoqSrzcdWR/UO2lCclKAqiTzeo1egIh0Xxym40i4Sk2NZJJs295madURxxPiliQRZianaCRDPPe82YfVxUVtP3gRLCo+ct9NTHr72CWHczNFbrYdmWHPZTzNKDVKAbMtFdSCfH8ET8xjsa7XbR5CJ/lTupMWOj69TR4nTUN2HT5+vgNNNnq9OxIzNhJlcmTqFxUJzpUUxmJnhy23girZhb0iopNcUzEz65CH5SgtJIignc7tXGuwoIbwv5HoAkFE4b8XZOSUqJvblWvUnMIOIonCdQasjKTAQDFjqstDAsNKNaJdhPMjOlxe7WY/vKLVjMVmXNPvQ2dH2pmlSGSJygkFgYWgoZtDaboPxhCgk2Ll0iesmVGg1OUuKiI5fb3RQmPVasWou9lW2w2tN7uZJp8iWEolCkG1T4ehn0uKWbFTUmiJKIpdZlGlD9/HrO7JDQOk9KUGQ1QxO0SEGe5UCC4h48sR+JfA6zPG4nRmYuhMBdDvZ0etImI3QavaDsenqdkYtfFFupxA2OU+mdhrSIenrL2y8+j2SgAxoKag/b75CZd3eiD7XJjD27duDdf/0TUSI+/o52dLQ0E9XXn7w1ZdJABiThb2rC26+/SdcqiVFzWo75eGhWZpuR8NGCXKSbS/sYHw/WktN3x1nXB1zMCuOJhJfeUn+S0CcVpiuzEIU/MR0Z8j6qaTFZkZtVIIbryn0OUxwJsfWTC4tGLbEhEprFYBK7DEVpXkprpyPbgE2fbcCH8+dDsnkO46tO/pGiwNNMwu/Xr58Y6JGVlYWZl10BrdWS6YeXT9yayBJTvm5IBg1efnU+dm+phTPLkpkaTdbDI+a4oSWzTpx9SImEdHqyDEMet3TTAi/jkl4mD5ojioPJ+CaFpEieJPRJ2UlBCiA2O7Pl8IlwSqRvlLWaB0lZ7aJ0kUgSDJKV2a3OzO9J0abFJ8kj2XR0ofE+qs5WQhZnJbLz8pNPItRZD43L9dX8xwGLylqaVdIPgydMhKGgCHC6ECYaHa+sEr3wij6/eDwCyyRmeUy4Xq/C5tXr8Z/nX0auQyMsnv0QZx7EYJNMrMWNPKKTGGlBqUVclRLVBUKUd/cZg0JRzkofjcc+USpObNas6gCL0kaRnhHHQwh52qPJ6hazIrqTISEIEXGTAPmERKmb6LSTgmI130WatFpM+qd/LUYLWVQrmXhS+CiGP85kOLMt2Lu+Ev9++m/48d0PEFz0HJJ6OhEBiV5zHhwciSO2ZRvkthYoTcZ0z0Z9A7R0Hoq2dihGDgfyPIh2dfFY6aPDawbC9KkIgt5u/Pn+PwO9MRgKeXNbSrA44Yvk9N4mKTM/QyVGNySFcmrp56DI2IjM2Px9EK1UzU2KloX4vxQnCPGHeG3+AN6J0eXvoViICAIFd1x2j3JclXGanBXnRWLBMMHgeXk6nUG0SPGoaj2REGaQfEGKTIwlIIKs02NR4pW/PIHKdUuhzitOU/KTERLnGIk8qEMR6HfvhImYqLlsMAwWG4ykrbbz5sE/ehL+9PsH8cZlVyG2fCW0ZMVah13MJD8i0SPU0EQC9EMKD5OQqjZWITffLnZfpPO2srguRh+RgGVBxRMC4jkA5gVlEsEb8Igs8oSrtkw8ZaJ1GCTLyVX03voTvWTVAYXCGCf5udgVpYvuCQQQCnrhIA3ltBHPEFdKWeB9hX0RuJweCyagJ9lHTXnnIRMNgyFNlTNUXc68Xu+wUvDbjT/94hf4+wfvQ5udg0hb23EPcme45KmVGr4F3bZtqFzwLtYuW454SSnyho2BIkpxYF0D2h97CRvfWwAe0sDh85JTz0LRiGG4+L67UTB1Snrf05cTyYQQikgvFFolnnv4CXzw+ico8pjRt5dFysRFKUG9k2n6z7t2Eklx3bwY3KbcReFNiKxPo5Ae2Z+iwx087odU/dIkTpww7RMUxT8NGpXCztSzkxxVhO8SEArAolWJJkuegReOBGEwWAUL7MPmL5uwlOkA4tayPoHyyfetCff8efIt2LF4Pf50xx341V+fg84eQ6Sn57iEpSZ/k2xsxJvX34IlHyxCQzgmwv7i5RvQ37gcuYOHQEvKZSblmXbOPEykmGzDimXYQ+ef39kGPTdKqjWHVuH4XpPxKDFUBeY/8xyee/RFeFx6UYFOHcaXiunQAi0UIjuRJJ+tlrTCx3tjwmIb6Hg/8+l2UtZ79GrpQcKV+pNB+n2CisnyTh1Qoc8Ec2y6kXgEFmJxvOg8qKPb2yFg0aDRC0fKI3MOxVqO0ClaZ61nip6Sxdi3voovX3OSIKMwS4+Fj/8DNocTN/zuIej0OkSaW9I7NI52YxJOXZFfyi3wkDan4CES8bMbb0P5qafDNqCQYinDIe+ZQ5bb88kCDJk4GvKkCaK7CAdUdVlIPHgBmhTeeuYFPPK7x+C2qaE3G9KQdxh/ITqt6NCqRY6CLCYBncqA5lAMgViKU2i/3N+6Jg/XqxU3aVTSU8mT5E/7BZVKLZZl5SW8p1YhRn/SF4aDxOqMcJuNom2ZHWVTawOycorEDg6uVx0u+RxjukwLbxDD4JNi1nh63BrPLleKblCGr3xnEi/9/hF0d3bgJ3/4A8xFg4COOrHf6kjWlaCAXGGyYMLv7saIIWUI0uK4rrxGjBzNlD4pcgki2d2BVE8Xks0NyG5rgKu0AMH+A8VwRYS9+5WBgnYtoQZiAfzr0afwtweehtOigtluPeJQK1G+IGULkuWoVOnNehpF2jfxlBm61r308QfOKWCKvuSrRCT7oU+W/0OSf5rbnAxk/r5IkoQTgY3ipGI66XU19WKztIJwlnvO+e6c3lCvgAUJaYvp65lgQXEKinc0dPq70qUASCKu4tE5fGGdwRgxNBMKlWG89+RL2LZqNa6++y7MOv9CMUEj0dwomk6+PFhK3Pci2IsYxU2ac85D6qMP0HrnTdDkFUE3aFi6v6GlQbR8yby5u7YKEsGlaeYs6PoPEFtA9xMHDTRmEnBbIx6670G8+fybyCYabrKahZDkTI+LQjo0TWXi5lMiUhoiWRwTmjVKtBP7DFEgSghy9aG9Ql/toTjgh55QKvUw15NyTBrhDP3RJHp6vSiwmMQ8hvbeMJm3WtyrkwNcLy0YDzI8cCMAC42rtNw87zSbxT4pzjQzTWcYjNHnmuizHBRIiloUWV1JgQ3dW/fgfy6+Br+47FJs/XwNVJ4S6PLz04zxMCkn2edDiiBLfdY5MJx9LlLm9E1agts3UwxVh2ivn74rAs3IkbBefAl0AwchSe9J01Yl+L6SGoLJlq0bcfN1t2A+CSmf4NhoMQtilBITOxWkZPur131C42Cfw5b07ZJkul5aA0KizoiYzzSfXrLi604OHxhHcS3pDjqnYTlGzaw6L4/DScGmJQKhI82329Do70WhWYtguJcERcIkgXT7u+F2ecTY0VTmNnF9owHcpMkpOZ3vU4tunHRCkg8D+UKVQiuGe7ClOvIcsBOUrn7lXWz46CNMv+IKnP/DH6CMmJwqFUaSmCFXVPdZGGcCegOi3dgwZhxZ06B0Kb+zUySIleT7lPT9amKVfI+MZE93WsD0HG/Gg0mHdR98gD/e9ftUx84GRb88ixhf0EccODvj0CjFujQTsrDAIGfutqNRC6bH8ZKRFLc20gWDnhtQ5SiRhiu/iSz+QYVDPsXeZOIM4vlvFFq16KRFTMgKshwfislXcRW3kWh7hNmfaCxPs0Gfr0tsBlBm/ArDIO8fyuaF4tqLuCtaOsUiNltnFoKF5zRqxU1URPaD6HFBkQNWet3Cx57Hz86YjYfu/Fly+7p1UDqd0BWWiDE16EsLZe4jmOzuYjWHlii6acJkGMdPhLZfKZQWq4A6cZ+MDJPRcQsy+aQ3Xnq56Vd33PV0YGdDqriQYiuV6iDLZUvhijYr1H6LSidiecSOPxxO3wmBJ7AlowjG+ZykC/jmt9+EoFRfptZ8Qf548iKXUXWHN5K4uy2UsBRwaxQFtDyJv4kgxaUjB8x7bQn+AsT8zASPHBRbLU4xQYwH6/TynFjyQTajKd1WRhfcnSmDMASKynAqzQQtOjV0aoUgGdwkryX/U2RKIdjtxZsPPb1zydKl95wydeqPT5s0cfb4U06BtqCEN0yJCcuxzNwiFpgcDB6x9KEhn6rIyoe/ZheeeOrprR9/umiKLeDPN+ZZrk9mSi1fdipi7LdacUB3Vfpvwh+neOy3mtajV7DbUDT1EBnre99UXUx1uM5SWcRIqYeyTeoX6nsij1u1hnkqCnhLbGZsJRho6w2iUG8hX6NFOxEKl16LKMVcTCI4O2HUmwT8cZcojxbd1lAjytWM74mkLPJlaqGp6fVJyOkZsuy32gIRQefN9Jk6jRvmYCiW7Ol5a8Wy5W8tX7RoXPnAsmvGT5hwwbhx49wDBg6kgDkX4rYAZOm8XTSZgd2+a+GbVBJNFa9ZsuB1vPD0s482eb0/z1eL4udlKdXhR7L1CUWn2r9lkyHbrk/3RRhJuXiraiDKw6aU/yGru1P5DdY5VUdqA2a+r1UqOulkL22PJqzE9mbnkAYVk69q6OpAYTIGh1aD2mB6nqukSEMEp5SCJDQOkh1WO0pycrGlbq+Ayr4MM0+tNKgPpt8JUe6X9pEOLiUwNCY15hhISSzqLITzSj5vqKv/fNuWrbe/+fob55QPGnTm0Iph0waXl2cXFxfD6nIRS9enW5b7rMTvx6Yln2HhW28tXrZ4yW/MbvfKIqcDoY0b+d4ieulI20u5kq1WpOdC7M9ciUGOzFo5mA9EQ6zVr8WS8jzlN9jJdkRB9Zk+L2WRU8cppLN7Eqm2iN/n8JitBIE2NPkDyCUfUO8jzSJKatVAjMgxZHZYRCIhVNXuQn5+KbE/G3rJEg2adAzFjph7DFhjk18auMBZaG4BZqvjc9CqFB1msuSkrxOtfBvwrGzY7fZgLJ54ecumTS+vWb1aazKZJubm5o735OWNzsrKKrZZLbkqlVrjD/a27qnas2Hrxo2vxBKJj/N5Wyf5ldC2benbrhoMOYezJjEPlxae9ynHMhEq/2vUqMSNyURKjLcaJeT5GpVyHjesHDKW+9sSlJTpmk2lC2SJlISpQVneUuf3CX/F1D2LoMBjNpHQvHDqLALCeANXTJkQhUQvMcJ+KEFZfjFWbt8onLCXCAr7Jx4DpyWok3BwFzlrayCa3l9lJKcfjSf9Eb4vAQlaam6CggiDzC3EBF0ut5tZWJTircVdXV2LGxsaxKZvIgLc78ZDoCM6sjC70yVeL3MxlPc8EU2X0qTk/2TK5AV0VNDBN7ovSWQsmufE1vaEO0iZ3OyXsk1GoWzaNBg8S9993bfVf3NEQckHLGBGW7aqJel0YoefdfMkfVrsarpgvul9c0CF5lAUeTwAMdPcEaDXMLrvbdiLgTlF2FZrQS/HXMr0PCTeSadjCNQoM1PI0srBC8QayxOaufBGluVjAsJ38VbUNwoCo/J4xOjSPqLAbJOsShz70xM4KK2QOvBOpH13SpOkSvqt8kuXPpW+b16BzTAuHE/ds6cz+D0S3B15FBhnmXUkJGar0i2kR08ovsXemxNF1sVkDaMVklTNJxlJxQnSwsijgLglFEN3PCGIQHrYb/pGk3uaayHFezG4sFTspbLpNaKMze/3kzCYXPQFxIyCaV+l2EZa/I9gLHEtOfD7RHeTIh14JngwYoaan3w37FFyicASCsqvJ8gd1eQPv9veG70g16JHicNI5En6gMKMISykb7sbSnUS7/kC6S2j9+qUqhvJ95gsBGl2vQ61xNg4IOYGF85wcMmeb6NaWVeFQs8A5Lty0vfGyATYnOBly+EhGRQMv0XU/HWy2FWkPXXKw6krF+pUqm90QTh+chIkc3KarPWCPKu+X4FN/4FNp/wLWdLHydR302t4slfNrU6/oFN+iLT/LGJ9I41qhSWeVJ7S1Bsrd+pV4gbDGkkhyvm8GTvka0YpsbLOgB8WrSwmMjOr4rgpmoieS9e/QMrs1PuuHkxgsknRPGRBLJDeWLJWo1SOiCbim3kic0o+4V1Z37mg+h5cCnqRsOTFhLi5pFIdjOEDXyQ5LaxKpUfHkLD4Xk+tXi8Fv3EMIGocTXQIS2KGQpZ1OwtJqfjumy0hKLkyvfsmHS5sEAG6nJ4G9l2e4dfK/kkL40aNYrpJq3iIVS8SJ8sh6uolKuyj31vFzVTiGJrrRLGD7xQdZ4t6RvimZGpf0jaSSB32Tm3fxiPdD7H/uyV8twL6uizqMBfKG7ekO9UaaQFZ2e/oMqf3jfbhGX2KVEzsehiT7xQkojsUecBl1P6MFifRV1jsCcfEqISU9L9jkfD/o6D60i/0WEmwNoP+nUy/fp8WfBQRDDctvd0fiSr16mRiWK6lzR/RWsVQyPSdqgQb1Kr4nrtB7GwjC1Qr/issiA2G8n9X4f+Bh+K/S/BfQf338TU+/q8AAwD3X2wl0b2tgwAAAABJRU5ErkJggg=="],
        "2": ["[尴尬]", "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGoAAABqCAYAAABUIcSXAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQyIDc5LjE2MDkyNCwgMjAxNy8wNy8xMy0wMTowNjozOSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDoyMzg4REQ1OTk3OTIxMUU4QTRCQ0U1RDA2N0I2RDJGNCIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDoyMzg4REQ1QTk3OTIxMUU4QTRCQ0U1RDA2N0I2RDJGNCI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjIzODhERDU3OTc5MjExRThBNEJDRTVEMDY3QjZEMkY0IiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjIzODhERDU4OTc5MjExRThBNEJDRTVEMDY3QjZEMkY0Ii8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+bUfiPQAAOV1JREFUeNrsXQd4HNW1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"],
        "3": ["[坏了]", "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGoAAABqCAYAAABUIcSXAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQyIDc5LjE2MDkyNCwgMjAxNy8wNy8xMy0wMTowNjozOSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDoyNTkxQ0VBQzk3OTIxMUU4OTQ0NUNDMEQxNjFEOUEzNSIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDoyNTkxQ0VBRDk3OTIxMUU4OTQ0NUNDMEQxNjFEOUEzNSI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjI1OTFDRUFBOTc5MjExRTg5NDQ1Q0MwRDE2MUQ5QTM1IiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjI1OTFDRUFCOTc5MjExRTg5NDQ1Q0MwRDE2MUQ5QTM1Ii8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+J/idvgAARDFJREFUeNrsfQecnGW1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"],
        "4": ["[期待]", "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGoAAABqCAYAAABUIcSXAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQyIDc5LjE2MDkyNCwgMjAxNy8wNy8xMy0wMTowNjozOSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDoyNEYyRkE5MDk3OTIxMUU4ODRCNEE1QTVCRkM4MzE1RiIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDoyNEYyRkE5MTk3OTIxMUU4ODRCNEE1QTVCRkM4MzE1RiI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjI0RjJGQThFOTc5MjExRTg4NEI0QTVBNUJGQzgzMTVGIiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjI0RjJGQThGOTc5MjExRTg4NEI0QTVBNUJGQzgzMTVGIi8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+/vuDuAAARihJREFUeNrsfQecnGX1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"],
        "5": ["[生气]", "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGoAAABqCAYAAABUIcSXAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQyIDc5LjE2MDkyNCwgMjAxNy8wNy8xMy0wMTowNjozOSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDoyM0QxODQzQzk3OTIxMUU4ODkzNEQwN0YxRjlFMTNFMCIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDoyM0QxODQzRDk3OTIxMUU4ODkzNEQwN0YxRjlFMTNFMCI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjIzRDE4NDNBOTc5MjExRTg4OTM0RDA3RjFGOUUxM0UwIiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjIzRDE4NDNCOTc5MjExRTg4OTM0RDA3RjFGOUUxM0UwIi8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+j1L2wwAAP7hJREFUeNrsfQeclNXV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"],
        "6": ["[微笑]", "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGoAAABqCAYAAABUIcSXAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQyIDc5LjE2MDkyNCwgMjAxNy8wNy8xMy0wMTowNjozOSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDoyNDY0REM5NTk3OTIxMUU4QkQ2QjgzQURCQTMyRDcxMSIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDoyNDY0REM5Njk3OTIxMUU4QkQ2QjgzQURCQTMyRDcxMSI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjI0NjREQzkzOTc5MjExRThCRDZCODNBREJBMzJENzExIiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjI0NjREQzk0OTc5MjExRThCRDZCODNBREJBMzJENzExIi8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+GVJuCwAAPvpJREFUeNrsfQecXGW5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"],
        "7": ["[委屈]", "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGoAAABqCAYAAABUIcSXAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQyIDc5LjE2MDkyNCwgMjAxNy8wNy8xMy0wMTowNjozOSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDoyNTNCMkNEQzk3OTIxMUU4OUZCOEYyN0NGRUU4RDlENCIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDoyNTNCMkNERDk3OTIxMUU4OUZCOEYyN0NGRUU4RDlENCI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjI1M0IyQ0RBOTc5MjExRTg5RkI4RjI3Q0ZFRThEOUQ0IiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjI1M0IyQ0RCOTc5MjExRTg5RkI4RjI3Q0ZFRThEOUQ0Ii8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+aTG/SwAARhhJREFUeNrsfQe4XGXV9TrTe729l9z0RgkhIUDoRbogiII0xUaxgOVTQaX5qYjiJ0gXEEGqSC+hhpBCEkgvN+Xm9jq9z5x/7XcmIVQDCQGfn4HzzM29Z2bOeXdba+/97tF0Xcfnj8/+w/D5EnwuqM8fu/Bh2pGTzj529mfokjUeuvofBv6cz0OPRYBEDDCbJ2u+sulaWeUU+IMtmsnsgsFg1FOpOP/ezfM6EBpq1yOhV5DLboDNDs3hBiyWz8zd3f7vFz++oD5TD00rPsdj0KMjgM1xtKGh9QStqe0ArWX0GENNI1BRDc0b4N2ZoRvoNHJZIJ0ComGgvweFrk3QOzdt1Hs6XtA7Ny/Q+7rn8H3XaW4vYHcUP+MzFrtN/1UCyuWghwaAbHqsVlV/inHfA79smDJ9nGHUOGi1TchSMOG+bsQ3b0B+xZswUDhGvQDdaELeTKtxeWGuqYNz6r6wW63N5tBQs75hzTmFDWugt69cWFi38lG9t+thSmmFEjQtTj0+A0LTdgT1faquTywiEYc+MgjN7jxJGzvlHMPUfb5gmLg3jGMmIakZMLRiKTLrViAQC6HOZECT24UxZWUIOB1IZnPIUsaJVBrDySR6Y3F0xBPo10wI0+0ZWsbAM2YiXCYjDBvXorB8EfLL3ligr1t+j97f8xCsti2aLwhQ2KDQPy3X9xkVlFaMP4w9+siQTyuvPtew18xvGPbYd7Rxyj7QqxswtHE9ooteReVQH8bbLZhUUY79x7bBM3Y8Y45bvUuGx3uiTyGH9JbN6OrtwdrOLVgfjmAtj06DBfGGUfBO3Qc+lxvamjeRX/ByvvDmgvtpabcgn3teC1ZIHOR7FP4/F5S4NzkiIbq44Qq6swsN+87+tnHf2X7j5H2U9fTPfxmG1W+irZDBzEoKZ+JkVE3dixpvRo5v8eymXrTHe/DvlSvw1dZJOGPPKf/xYzMd7Vizfh0Wb9qEZaEw1sGM9OhJCOw9E558FoVXnkHu+UefLqxaegNSqX9pZRSY1faJCOyzLyhxcfEo9KGBgFbffIlx5iHfNc481GWYPA1hur3Bl55BWd8W7O2wYlZTI/bZZwZQWbvt5V1DcZx6y3OI21NIO5IIRgz4Fq1vyVAPfnrcDPhtNlz/0goxKVxw4KQPvo7+bixd8gYWUmiLIzFsdPpg2e9QVNXWQ5v7LHIvPLE0/+b86xEJ36YFy4vgYxcK7LMrKLEg3ijjATSP90eG2V/4sengY32GiXtiqHsLRl54Eg3DvTioIoiDp0xFzT6z3vdtuobj6B5JYHUkip/c8RwOqauHyeFAeVUBpx0wEfcsbUfn5ggiRIDj2ty4+uBZMGkfTiOT7avx+uJFeKWzC/N1uryZh6CS8cy8dB5yTz24vrDw5WsJ9W8gHdhlFvbZFJTRCISGoYeH9zJMO/B3plPPnW3ae38M9nRhZM7jGBXqw6FV5Th85iy4x07eobc8+/oXMH/lOpx71HQ8ubQbz/3iKPxlxVuYv64PfzvhMIQzaRxy1z/xylmnwU53uUOPdBKLnn4Mz69fjwUZHbG9ZqGKbtG+cjGyj9y9Lj9vzpXIZP6mkRbAYNwp0PHZEpRocp5Qu7cTWkXN/xqPO/0Sy9GnIGyyoO+JB9HW14Ej66pw5IEHw9o8eoffNpTIY8Z378J+e1bgz988Cvte+jD2nepHl2MINx99NKpoYZc+ORehfAI3HXPYx7r0dS88hWeXL8MrsTSG9piJ6mn7w77kNWQeuG1uYeGrP4fD8YIWKP/Y1vXZIby0Il2IZ2h4hmHGwbeYv3z+eG2Padj8+lz4XnsO55b7cdLpX4aVsPkjyz9fQJgKMLatBjaThkuOn4Kv3voUvnxEnRLSbS+swx3z1mHZj770sS+/7aAj1XE8Qc3j81/HnMVz0TvrCNRdefN+1ifvn5P55603kUxfrFXWJnclQty9FiVCGugVi7rI9OVvXGc96WsIked0P3QnDkiM4DtHHImKvWZ8/PfnvUy//GFMag7glrOK1zz+8nthdJkwq96LG+9filsuOhbn7j92l+ndlnkv4v6XX8YLRhsMJ34NNZqO7J8u78i9/uLJtKyFMJk+EmH+9F0ffbfeswWa1/8Xy4WXfQsHH41N816F+6XH8FW6uRO+crbKHOzsY3l/GFc9vggnTW1DwZZDOJlD70gGf1u9FFcdtA++NH70J+Io3nriYdw+fz7W73so6mcchPSNVxfyT9xfprm9IwrR/lcISoQ02Cvx6G+2H1xxZnLMZPQzFu3Z3Y5vHHAAGvY7aNeuWj6JFxashs9uwh6TR5EoZRELJ+GqrCwmdKF9Mm6d0P53t/4V82afgGAmieyvL2qBybJRucD/hhilR0aglVVdZ7nkmjPTYyah9/Y/4Xy3CSf+4FLAYt+5N6cCdHVswpaBAXRFoxhIJJGg8pktRqSyeTy48FUYGCf8DjvSmRzMBg1eswkeiwUVbhcqPR5U+oMI1NYBkt/bmUdFDQ6ePgPPEdYHfL4U41Pio1jTp5uUFYtNxCuMp5xzEcZNQf8jf8fZTgrp/Is/9ltKJmHZyhVY0deHZaEIevJAxOZAhmjLWN8EU6ACJqu1mJ8r5JVFF7JZ6BmuXZpHeAT5SAjGnhHY1nfASwBSQ/DR6HJibFkQ4xqbEJy058e6tgRjrtVq521LNljP/fdkz4VT2OyjNH85okR7jUM9OPWMMz76+3Bhl772El7v2ILlsQQ6NBMylXVwH3Y4HNV18KfoahgDpYRhpiB1Lr4hm1EpQ42kWjLoBS5ggQIt0HL0hhZIslXzBZDkOSv7erCwfRVMmzagfNVaTHz1VexdUYZZ02fCWNf0EfCSoehe87mUXsgntP8eQan0kE0yEOnQCCq8PsCz46BB7+7A0y88ixe7erFSMyNV34LAfnuhkpaTp2Vkls5H/rG/w7J2GSp7NqEyMghLMioaDRsPAxdOp+sr8POzFJZutiLt9CBldyNB5YmVVSNeWQ80joGlcRSsMw9BlufO4fs+u+wNNK+9E4dXluPEE04GhB/9h4eFyNbIz9Vz+bwCAJr2XyIoyLUaTALNCznGCK1EeP+jBY3gyUfux5OdvVjl8sEx7WBUjZ2g3FimYyOy994M0/znUffmfFTGQhjj96EmGICxsgJpVCKby/DIIZ1No0Cr5jXAwOdcLotsfBBauAe5juXIZjJIGMwIOb0IV9RjYNRkjIyeCs/UGbCfdzFDYA/+8uyjmHvDn3HapAnY+7gP52Bmk5mfo4ui5PmU/++xKEFYeiGjMhGi4aJhkjr6kMfq5x7HbXPn4g1fFXyzj8Gohmbk+LrU+tUwv/QEnC8/DtfKRai12dHcMg5Nk6bA6WSMIuHN0XoEnlj5nC/kkMmkkWJcSqaTMJLjhNNZ5M1OuMyiOBlVynfxvAoehYFN6Fr5BjS7Dd2N49C9xwEwzjwCtV/5BtatW4WfP/MQDl1xGb5z6ldga3l/mG8ieDDw0LPpHHKEm/81YGKbWRlQ4KJZxYfT/bzvIxbB3bffiPuG4yjMOAJtbWMYW8yId2+B+dmHEHzyPjjXEXa7rQi0TkCAoKGpzEd3Y0AkmXxXrleTdgk4HW44eOQplAQF1jPYgXq3hULT4LI4kKeVpfi3glFDr7GAiN+IyT47Ggbb0X/fG+h87p/YePDJcJ/2TVgu/BmeePIRrLrpBlxy6CEYc+gx73V9RJRGPc8Yma9EKjWDyG/erhCWaXcYFLU2LRYl/trwQW4vMoxf/v43mFPVgsaTT4HLbkcyTxfyzAMI/OP/4Fm9DE63HaaGOtISGzwEAk1lfpi0ItLS3gM2lfvhOjE+8bna58OLa9tpmUaMJZDoHhpEMhWnseuwUqBlbh82xYeURca5sDl3AHmLF5PTMTTdfx0WLX0VPV++EM1fOBVDbePxg3tvwo+Gh7H/l858J5jgexljUeSa2syGKdMfNa15q1KvqCko9LkzVHR3uD4uWUwJSmLr+xHs8DCu+uPv8eL46RhLLbU7nYj1dsF81fdQf8W3Ubl5Fez1dTAFK2G12KigGhx6Eg5aUlb/T+xAh9NixSA51sINGzC2qgI2mxPlgSpUlNXATQGJO84RaIRTGVS73XARHWYpwDA9V5ocy1TfhrEb3sLev/kmMldcAK/HD/dFl+OXy9fi+btveWeMErfOOGjlXRu+dE5ZvqbxRoQGsbOg4pMXlJh9Kjmip9NKs43vvl5C9qv+8Bs80zgBY/eeQS22IjnvRbguPBlNj98HW6AMufJa2LmQZlqjRo2t9bhgIkXpHOhmuKMGf4hrkb+57VY89tYy2MwWjK+qwkgshlw+q4Qe8JWhraEN8YIBIV5LGc+VOpXRaFXC8pIYixC3uCrQQNR3+L//CtOlp0OjO6688Oe4alU7Xn/k3rddFMFEjCBmYj6Jw6buicFDT/i6Fo+N481/hgWlhJTg1Vt+rfGGNZq/YXsgwfhw4w1/xNO1YzF29hHIcEGy/7wV/ktOx17JQfgnjEaMiMxtKPby5amVQQZ6v81CuG1EPBnH4HA/F9WkBPbudJj8K0DrXLRpE5Z0dOHA0a0oiH1vzTTRylPkX26XB2v7BxB0utBGzmS3O+lO8wQGRlj42WYKuMBzlyfyKB+zJ05Y/SqqfkD0t2oZyi6+DL+eOx8dr79cWlBddUulCGxO89rRePjxiI7b41YM9WJnYtUnK6hsVj7ggOzBx56dJdHUCIW17dL+z993F+7RHBh96BcUbM7d8nvU/vbHGF/uRcuUvRAlcCIBU1aYoUa6KewyBzU9X3wPEwUUiY6gf7BbSUUWdOtDrDfgcKKXRPm+RUswsaYG46srEU6ktsUzge0enjNEga/p6sLkunrYHS5UldcgTmDt58/VtGa32490PIaRvm4sGY7CO25vHD28DjX/cya0Te0wfuNSXPnA/fQOIZQ1tigeNZQgyrSb8cXWagzvf+QMfub+IsDPnqCo4VpfJ2JT9r3LQ60KxsPI8ELzJRcQWrEU17+5HHXHfgm624vUv+9D/U2/gb/CgebRE2EmzhkMR4jMTEpIBL2ocNoVWsuVLEeQnQgnFo+ip79TQXELXafEgyAtKZKO496FiwgoDDhs/Bi6soISzrbcLaF5BWPQa+vbleW0kYNFEwmMxOMIJ1NoqqyB31eBoYF+1E7dA3sdfAjaV67CUqJS38QZOCLcgcqfnAkHLWXdwSfiwQf+QY+h8RrMVFJCf4IcSTc3zToEkTFTfit5yY9rVZ+IoHQJnPT3uWDlpdpxX2k4u7EWFSYDElxIp9ujStvXP/kEsrO/ADcXKnHfraj588/QVBdAeVUdGhk3NgwOwcjLs/LGhR95qZ0uq1n9/O4wJ8LKkNj2DXYpV+ihsIYScTy4eAnW94Vw7JQJqA/4aE1JJVxl7LTgan8ZNg4P4rXVqzF79DhaqMY4ZsbGwWF1TpWbwqbr7tiyGTUto3D6H25BU1sjltPlrQ6nUTN5Oo7sWwv/pWcgOHUa7jN58BbBRYOj1PBSKC7wca1NCM84bDpM5ukf16oMO3aS/pEOoxwjg8HeWUf+5qBZ07FPwEWimVFZgOqKCqx/8RnM0WyoPXA2Qo/dj+arL8A0rxUOfwBjyusYi4xY2dOtMt4S0M1cQI/VpKD2B/IME4M+F8FUyGIzhfXwksVY3jmIA8eMxv5tLQhRSFuRV57v47Y7YCFhvmvuy2gJlmEPKtNIPAErBbWegqpwueDl50elcTMRRSYShslXjS/94kpYaClLNm9EeyxHsr0HZi99AbZrfojCwcfhr90D6A2F4LNatlnPUVZNlXKSbZOulkbSj4MAd0hQ0ha844cOU3gEhYaWX9r3PxzHyydYJdLQJVDzh8nwH9+wCb6jvojovHlo++svsU+FF6bySlpFFpVEVu1DQ3STeThJHsWCXHwWvpSmq9Le5yYVXyKKqyFCjBN8zFm3FpuHYqjzejC7rYGKbSCxLVWhJDsi1kKy/MSbb2AkEsdRkybR2jOwU0iddLchur6JtdXkVMAILTMUz8MnrWHIoO0LJ2Ha0UdjuKMX7cMhxI021JGYj3/8LpjmPIrw2T9A/5jJsDBebRWIRM59GxswMnbKQYzbVZ+YRTmopTt6OHU+hwcs/ZNnnL/P5D2groqrZJM6EDnKgtdexmq7F5bKWpTffCVmxLpRqKhGghZX5gmoEsGanh64KVwRgJGvE5cn1pR/H0GJdQjYqKQb60pm8PL69eijW7KbrTiguQqJ6BA2dndAqg4W/k5sspqLvrKnA0+9+SaOnDgZlV6XEk7Q48HiLZ0ELWbU+X0QJztAEEGjQqBK7qRIWmefdR7KfHSRBBcdoQTcVLJxHjs8d18PA12lhSDEKIBnO3Czt9UI87gpQHnlaaQrn4ygfLnUDh/B2BCcweDMzB4zTYe4bcU34KLbJbXCmxgwWRGbsDfMD9+Bmctegb1xFBffBAf5R1NlPRcmjp6RYZSVcneScrIZxSJ0or2cWvC3hZQn2LAg4A1i1cgI5m1sx3A0Q4Fq2L+xHDUehyKxkVgIvQNdGBjupdCt/F0Cd899FVMamrFvayOGYnG46QYH4kls6OvHPs1NCvikGMe6hgbh9ZuJ5ppojTnkujej4aAjsedRxyLcPYhNoTDSugHBugaMWb8U2ScfRN4bYLwzviP5PN6soZFxMNkw6kRdrO2TEFQgm9yhw8+jkosxUlZzfsPo8Zhm0baV4i3019mhfjimzUKMbmbywzdjbFU1dIcfXqcbZf5KamkQq3t7VKwRKxJ0ZyMIMZYy7iKonHAxyRsWcvDK/iaLA/MJrZd1diGW0JHM6phJIbUGPSouSoLUSnCR42sjkWGVzXpg0UK+pwVf3HNPFVUzjDmVgSDmrFmLIIHAuGpaD+NkXzSCTV0dqGtpQc3osUWApMCAAfuceAo9gBFdwwPoiSbh8njR7HXD/fwjyPB3Bus785lWeoKxtMpoQ9ssXoTvExGUlS5tRw5nQVVRvRtr2k6b3lCP7XPkdpVRziAxcW8EXnsGhxA6O5snIEiEVxaoVFaRpRA29PdTSJI0NahMtJWCKpRShsKNkkSOAlYkNxch7H6FQb2DCJFrRQso4MDmSoyv8JGDZbclA8RtSuqqtaYRb3T3YlP/EI4a3YhoZAAbe7Yg4HKoeLOmuxv7tTTSKsN0wTaCEr5vOAEPwYarvAIFQnaN15Tv34LWAw/G2P0OwHBPHwWV4O/N8FRUoWHDcmRemwPjdrWrQolOtJJq5GsaDJrLI8La9YISuL0jhyWXxoA7eHZhr1nYz23alh1QAVVWOlhBuJvGPm+8gPraesQLuoo5EnusJhN6uUC94ZAitmJNVpORIELbtuDi9owaYw3j2HqCgIWdHYjE0xiKF1Ql97BR1RhT5lFCEle5VbiSEG3gIq6kMF5cvZ7nNcJnM6FjoA+J2AiMjCWPLlmKJpcN5nyc12TEIF1we38vTFmgYfJUwFtOHJApQvtUippXhsmHHw0b3743FsVwKgcnkWIDUY9l7jMU3HtBTxv/5mtoQb6scoYuGZtdzqP0HThE6xl4e2tHnVM9bjJa3w1IqIlphxvBVUswle4xanMqAW3NcNvpKjYNDSPBRZDmyTRRn8Q1s8QngeV6XqV4YHbiDcaRlb1dJKc59EezqGMgP2ZMHZ8dCDEmFYVEYsz3l0JlXXkVVgyF8cSyldinpgL1Pqc6T5DkpFHj8dy6DYjTLe5RE0SKwKeCCrWypxMDJLqBSg+mH/9FSie5rT9PUxsa+jBh9sFoGteM3r4eDCTScNhsCMq+rHXLkFuz/D3L2EqK4Ro1FrnaphPUVtZdLSgpY/+nQx7xdHrUQMukSdXVNbBuB53lIdoWJUoavWYJySdJrl5M4ciCSuJUyPDGgUE4CSokJqUI1R2MLQaB9dRON4U8QO1eQTfXRwgdjuvkOHlMrwvi0NZqBeXDqezb2atcBj67HZXBSry8pQfPrFiDA+qrMakygAhjV57W0VbfjEW9A1iwZhUOaq1XWcAa8riO0AhWdXciO5jEtONPQtme+yHb3/MO/pMh4HG2TsDoWbMRH4yrlFFWN8LBz6xgHIyuX/X2IvN1IpbfjaSQmPs8LJvWzaH7+3TqUSZqfNRoOSlUPwo1Dtt7C2qEz/q8p1CzeS0SDiMGRgZgt7vgslmKe9bSKQxEYyo+yXoIp3LQ9QVsXnSRiHZFkgzuUcL4PA8dHouByK4STT63WvhsvtieIKDBQvBSxbgiGfEHlq9BLxHhkaMa0FCypDzRXFNVLd1nAo8tXoLDmmvhNhtgIDBxeXx4atHrGKSgqhnvDvvO90jgorT+QtGStsWd4vOYfWfBd+cd5FsxRDM+InIzGp12lCUj285dRIBzQ/tmdPz95lzdcw+dB5vtb2BMVqWf3S4ocoNQVdPh0dGTMc6kvSOIKtdHDuV4ayEc8REMaDbEGKtStKI8QYWQ3DQXIskF9zrMypNK6khARCcXdvVwmDwnTcisqRrUpAobatxmOCmscLqYPywoNEi3RfRltNjwZt8w5pJYlxM9njKhldZpwnAyTXeno5zEeC1j3NPLV2K/ukoK0MWFTmBqczMWEOK3d22GgRZ7wv/+D1xNE5HevIIg0PieCjLC/WjZexoaxo1Fx6YtCFXQrbrs8PMzRvN+RRB3dg/gxvUdcN/2+3kNKxaclS+rWqvZnR9JSLtUUNRnpzGfn+Ggtr5Al3SIq9gfsVVYRv4+0L4S7joGZdUgaVfBeSg0oLhNbzyLDGG7x29XJXQaDZYPhRiDErSuAjxmM5r9VtR5rQqyJ/i7AVqandHeRcH4uTBmusoOnv/aqtUYikSwN+PR5OqgilXCp+xcbBs/a8nAMOZRw/fl3ydUBtFLVze+eRw6KLy5jC3xjgi+8O0zMfWr30Kur/0DE6mZWAw2crGmKXti2aJVCPP6G40eWL1EpAM9ePo7p+LujB2z4gMPZ1e/dVJP80QVa8Qai9ap735BJRzuyeOSww77H3+Ih2OX4YdHHourgjaiPW1bAC7jj3a6Myl35yUrKNVXgopoMobVXX3K7YmedUZlY3QWYltVJL5NlW54LYLsCiTBOrU/TY0WjmWGj393OjzoIj5fsr4bW4aGyGdcOIhW5LNb6FKzKrvh5XvH6E4XbOnGhsGwiletARf6wyOMVS2q7vX44pfRu7wfs06YhROvvpZBdxA5ghvtA5pxirlHK1HhHrAY/060SS/BG/B6vRjctF4R7TPp4lqMhb75LrrxaJdKsUVMdsRI/A36bhaUXkyh7UVzwNTBLlhu+AnuSybwzcOPxY/LnRjNmOXx+tHARTUZuNhkQgWqlanUHNkVGsIw4XAir2FjOMW/agjSVTXSeuq9dCV2h+qLkDhgoaCr/QHlFqOZHDZEC9jUsRlhQvtyhx1fID+q5WvyFKggR2l8iVJAmyIxvNU/zBs24AgCBz/5QprntDaOxgDd5zMLnkdP+wCOPPcEnPa76xVwSBH1aaYPXiLl/lIhtOyxN2qaKtFHohsN+BnvTEi7veRiZrj0LFKBmiPNFHhlKqK8TEUmhrxm+HRcH0nolBwXP1JRh3Ej/Tjnz9/HfT2bcd4JX8M1BDgzm5qwz8RJSGxYipynjBCa8JuLmMwkCAZySOYMtBoNATvdk1GQIFDlsiiri3Ohg24/nE43hqgAm2NZdJKIDnDxNZLsWpcVE+oCqizhJFHN5HTC7LyyoN54EmuHIgQgOYwO+DCp3MNr5Wc7/QiSaC/v78PTr7yAbFjH6T//IQ77yTVc/BGkurZQSOYPKV4XPUKebrNs9Gg0TJiMBc89h0hjE/wOWks2RXBiw3AsgTrN0ORxuKtjqWSPSW5ML1WCP8JmhV3ZhVSnacVi3IivHHXxEM574Do85HTif9aswKUkoi31tdiwfgmXSVPdPwX+ly3kEOfC5ggmanx2uKmFDHHFkjldhGi0aPw6usaBeEoV9fK5HFGfEeODTtS5fXSnBrqcPPplmksiBakr98aS2BKOI53N8xwXZlS7UWYzqIJmdUUjY6CGp5Ytxbx5q9DY6MHp//cHTDrpa9BHOpFmfHu3kIoJYgPMHo+ytmhfH4y8NvFeRgpdUkyZR59FnFaetlIB0wl4nZ4iB+S9uB2ueuQyPU5tq4C0TwtMIFjs4NVUC/GIwws/b/aM5+7CnREdL5JAlk+fAps/iJC0gVFAWemDKIjFFNQNd8RIGtMFpWtinf2JrHqvNC1DMhJl5CiT6qpQ7rKqkkOcqDGWyWOQcSieKxAw5DCciiFJ4VjoWqoIk5sYrypopVJicXmCcDlcaB8axNOvvYyhvjwOPPEgnPLr3yAwehqyvWuRl3aBrTFJhMOfTYLgpOA5PISlCxZgkIS7oYUggl5iayqoormFQIVhja8X7yD9GJKt0UqZHUs+640JV3SVk3cWu3bf7/HF3SAov77Vb5fawhLkSU6DCeW9mzGFEDYr7cUM2oL2HCrFQiEUJH8njSRCmPOIcLHlZ9keY7dLj4QFVgZeKwOapJk0o854E8dIMqUmsSRyBVU3ktyguMygxUJwITC+AJ9NQyW5k99bRu23KFT2ysLX8friLRi9ZyNOv+Ji7HPmOUq7U5uXKcBj5uuNVAj6WWU5ucFBLF24ECtXrEBMsuweNyZMmIhmCkZSUwrV5pIob2qFt8yLKK0673UU01eFoqCtdPNdiVTm2ZQJeb+3OKTkI27I3iWCEjBHvfJtn2aUZpUKuonlgwnVx9DCi49Eh5AzOVVPuJNCyOYztIgcuVCenIguyWlSe5qkqpvh/UuuT6B8hJYlfRP5QhK5UjZDEJ/XaqfVGKmdWRWrbLwbeR/GA4UETWarCtrdXOAV6xZj6VvdCDQ6cc41P8EBZ54Na2UrMNRBq+BrfT4UKMiR4WF0r1qFrq4u9A8QHJBki+JVVlbhoIOmoW3KFFVmz/A8vYgo1AigQHU1/BVVCA8M8forYCqVZlQfIz1CauxeV+Y7Ou81DPbd7w4E+pTn0Xc/PPfxU3357T7YTO0U62knZ2mrKIOZrstMrY7SVUjPt0bwLYFfM6YZ6CMQHTSZ7ar5UbLjeRJoL6+ugoJI5Q0UUJFAy3+CnOS1XocTNkkzSZuZBHbGJmkjo9NRiLCnsxsrV60g6ADqJ7fg5J9dgBknfQnlk2fRfHuR2bKKypLH0MgIRniEQiFEGZ/SJOOSVK2rrUULSXDTmDGAxCaek6aFYauAtsJ0IjpXsBy+ymp0bdyommikQ0p16/L9k7zvr37jm/udVDtqv3uuv+ZXjz3+eEtVVVXo04hRQXkvuUAJ1kIwy+ki+uMSR7Jo9DsRioVhsbqQSiUU2stIxpzkM00hSJGNGAKb1q1HwemCqbwamsOPAcLcEC2PHozuQ1qPNVXXMqA45ygmzZp0rVlKUZpVwuFh9PT1oKunj4gPCNRXYPxxJ2HygQdh/IEHw143VpJ0SHcsL7ZXG4sb3KSc4mMcqiwrg8/vhy0gk8VsyhJA8JKhVRXC4aJb305AW6vNOZ5nKStXuxZTiYwSlJXvLZ7DShfqJp2wkKJYAmU464yv+F+bN++cRCJxrd1u3+2CqhQtz3AxpTxhoE+2WR0KqVUSMtsMsrEhT15TUNtgql02JShJG/WHIiSBOZxxzR8x0tuLNx97CH1bOjCwpQ/9PH8tikhNYoeJgEDKHqZiPzvy6RTDQ0JtFhFh+7nAVYwdBxxzPFpIQhsmT0Fg/ESezEUP91JAK4sLXco0CEKVxXJ7iglSQZPq7ySuW7uF9FJDjfY+LfUWvlZKHgUBFAYLvHSP0mIughIkqnEdZDRdjhb17IMPovm4NEa1jkFzU/M+S99cik9DUA1y4SKkBG+wzuXGCONOdziCaXVBJBJRlJE7bWGskXhgNDhVQE0S7Q3T1ciWmVHT9oGhog3TTz4Vw5vaMbJpA6IknHHGgsjIMOKxmOpqlfSLLJ2RgrPzc6SoF+AC+XgEhcPU1NIRlxeXMjaCbFengsjadgISd2RzuaTfGUlaiolWBGkVMPJ3+ThyjE3pZLG1zOH1wVJeqzZwZ7q7FW2QJlJLXT02z1+gkrBVDQ1Ft0KLElQvjTkF8RoEFl6iQykoMmhi4WuvY1RzK4JlgapsNvupwPPmbel8IrqCyYf1/WGVkyuzmxCnoJxcXSlllNmsqpvIaTFhMJ7DSCiM1gnjioW2oY1qUQOjxiAwZS+JdFu7I6gF1HIGe8WEhdVLxsBgKlVqdIUgi1Muo0h3bsRWqrC9i9pqITavF/0ECz3SRCOV4iVvItjShkVP/BuLX5mL0OAAXVhcxSqx3CkHzMapF34bdXvuxzjVQ3zbgIHFc3DTr36NS667ttimTUDjKa+glVkUNDcY7DDQHcsmuRyPfY85BrE330K8cws8Xp+toKat7n7CO76YndAIIEgG6TX6ogk0+ggRaEEuuxNDqYyC5z6XV2UMZHtKLJNCKqnTAILQGCNyIxGYrBZkwiHo5CwoISO5HWuQYdAh4wdkV0ii6JoUb7QXhZSIoH/9elqZC+7ycmSj0fftWjIzLg7TSl95+RVM2GMqWqYfiB8fuh8eeX4+mpyyP9sMM63L5S9DnErk8Plx69/ux508fnXt5Tj+ez/ChoVzcP7RX0RbYwV8Yyci39cFI+/JRet2UAnSVBiDx6Eq0o2Tp5Ki2JXFauLuqch5TQtlef+CSPXdLCi1hTxNNCf7kLJ5IjVaQLXbT0SXREWgEkOM7m7ynCIyJBriRYaSabXGAZn/YPViqHsVYtEYWg88kIisQ20ltUn8CNQjvOEtvPbEHVi3dCmG+/uQ4WLYCTxaJ0zAhFkHYqRzEybN2h8bOzow/MYb2G/27KKtvatpU7Q/SuR2wOwDUD5uOn53xol4iUK68PsXIRkdxtIXX6TLiiI8FMJpP7gUp//kMnQQOf7ilJPw/e9fjlcfexwrXluIpTTen//mMtW1Z+Q9i+J4A344KdhMgnGTscvX1ILqNgKYte0g2yZwIRkmKnUmo5XTYl0oj2O3CkrcXrPqfWAk9ZO/vNU5Aq+NAEDTVSomUTCSCw2h1W0vNULq6jmezhLGM6RUVCg356mpwbVnnImT+vsx/ZRztu7Lwb1X/Ax/+vk16OO/GEVQ4yI893gRZ3xb+NLL6Lnij6Ax4N8bFmPy7IPw7N13oX3NGoylENNEbds/JPbUEHaba8dh/j034Ka7/4WHX5+HgU1rcNXXzoXNYVBk1Gy04Cs/vVy9ppGA5G8r1uLyU47HTQ88ir18Ntz9u5/jgHOKIxgev/GvKKuuwvQTvgQHryvLmJoaGkTbkcfQwr2qPQ2MpykKUOhDcKBran0+8WVHNvEPbTcKSq1oitpSR3cRTuTQMTKE8VUeVRis9pajW9q28mkiPROfzXCYzUWiS4vycNH91TXKpdmrRqvUzDlfOhff/9EijN57Bp6/607c8Ohz+PoJR+O4b5xPizDj3t9ehc71G1BZ4ybs9eHk7x5BipPArJY9cdejd+Gwr34bIytfRzaZfN+MtwCR1OBG3HLl1bh3zvPwBwM4c9+z0FTtgre+knA7g4H13Xjout/ipIsvQXf7GlrYMC6//18YOvgAbHprGcGCCf/689VYt3ARHrnzIdz89P00Lg/deAW2rFmJGJGsob4ZjiwRb0WlGqcwkxav4tlgP0/1/y0Gw4vkmz27S1AXbM2LBak9Czb1wcJrsZu5IDkNWQbSSHJEbRATSJ6jdnmsPsSTOVpZAl6vA+5AUEU42hi+fsWVeOWxJ+lWbkArbkAvf3vTX/6I47914bYP3LxyGX7FWDGm3ovuTVtUZfi2JW+grHkUfnjiGXikczr8TU3U6qH3bYE2kC+9ev112PfEL2PPgw7GOROa1ae7q8tIwEnFjTYEmipx3fcuxdrFb2AdQUB1Yw2uoMJcff8DOL6qEpd960folF0pPH777TMw7vDjZUQNXbUXmqTK/D6G1Crkurphb20jaEwgOGocOl57ATGCmAp/mXlzKnELr+YLn/huDnqvs/jkzRDd1PgCyko6afbVXHwhoAH65bBkk1MxZUWaaqLMqoy5ZLsFXLgYfAN0Rc/c9Cf0LpxHKNyCe3lzF3/lRIiq/eyyHykh9XVs2va5XevWImDlwpLQ1k5sxIZVa7jYbfjK976HYy+8AHf87McKeHygW6F2Z3UDjv32RciSM3W3b6I12dSWmTwtIE+SbhXKMKkaD991H55/axUstmIfiCtYgcmzpmHmtDY8Tiv6903X4jtXXoF8SE1Ng4sxKkU066ypg89iU7HUOXo08rGYogzD69cgMdgHL9fGYTIfTc418RMVVKkp8mqtVOn0O93YOBhWDZMuxqek3CxJb5Ta7jAC+e32NEkqSFI8ObpGNzmMb9LeDNgrce6hx+D+q3+K+377G5LeLkykIL52+TUYIXi48vQTsezVF/HgH/4Xj99+Oxom1DE452AkHK5vCmJ+ew+evetWXHDtn6Bb3Yi2L4HpA74hoEB+1rrHPqhgTAwPkesdOglHnDa5OD+JrluSqal4GvsdNQ7nXTQLTWQJyXhq2+uPPOt8vLWiQ9qzcMjXv8cg2wCj065KN0Jih2lm5vomWCJhBOjWzeRXAtWRT2GIblEll6UDWabI6IUrPxHXt5XYGjXtoqDZXDVEjQy4PSR5wIaBQVR5bKrJxGq2IJLTVZm9ltqY29ZFWRSyzCMX6G6XEkIhi1Muuhi3XncrvvbTq7f1BF76i0vUcx/J7xtzl+LN/Q+CtC021HphJuTPkedkUjn4qr2o7R7CSw8+hMPOOBczT/oyOlauwIQDD1Lpnff0OhCR1Y1qLbUIaGgaV47ySiMqKtPo3BxWlKC1LQC3ywJ/ix8t9cKh697mIvvOhM9pxblHnYxRNT7sd/wJsJs0nPeHP9Pn2xEi7asZN54QP0CrewouLs7Bhx6F8OaN2DDvFTRU1qrSh0dKL2brcYlcptpsMPbsfKfsdkcpQ+7wGI2/s5Hpm4iQgk4PeqNxlePz2EwEEVlamAchNRMjqbLgOt7OZeYI+QTxGWS7p8SnaAReat1tT/wDd/7+17jj/pvR6DFi7H6zi+3QREx+t0HlA485dhymHzYGsVBcvVc0lER1gw+nntWKxtE16vyx02fBX9sAPZ16f2UzWWDWScxTcXiDfjjcBABr16CxTsfkKU5MmOzDHnsHEO7jwi5bhpq2Ghx13je2vf6lB/5B6JPHrQ/fwb+Nxg033KFKNwajA8Mb28kbgYGBISwgTUgEy9FWTyHbfFj98gukGauRtVjhsDlVz2LQZpe1OX+XWNT2vl5cmNtkvNFntZmkEFYuGWze+PqBHvgIGGzE25FUnotKYhlJFSekbJdVlwSElDEkRhn4axuFAIcQQiPGHXUaD+CP3zxLtRVPnlUUVHVzKxfBLuOKMOOoMQgPp7B6+QB6e6N0ryZ43BZCfB9M7lKfu4uLQBKdSabev4NINhmQ8+UHt8BWNxbTT/w+5tzei0BNA2zuMPrpdof7shi11+EYP/NIBBqmIEgryMpOGr7dQFcXglXV6Ovsxq/uvh1XWW1wlAew5cm7sOjBBzDmgMkEI6PQ3NKEGQccrFR8pG895t1zOxqJANMk+tA8sNErZOJZOE2WsxO57OXmD9k2ukOCMmzdqafYjj7Rb7aeobqHSGa9Lj+idHGDkShaytyqO8jFC8/LOIBkFPUOyzt27ks5O8OYJuUFuenyujrehglzbr2FqHUIt/zsSvRzQY6YvSesssWylPycfvSx2Lz0XgzIro1QCOPHktXnCigLSqPmANrf6sCMk4v10dzg5uL4tw+cwqyrFmmD0wcB8DWTpuGUa15QU+PioSxCvT0w0103tFRsWyCZZyHcOctrPu5H1+ArX1+BeDSKPvK1tkMOpWkPY8H9/0A2mkFmJIrJo1vhnT6DxI3O2lqOe679HfT2VfCNn4QBepM1fd2YUttI10xAlck2xLLpvbjSb+yUoLYWuOTJaTLeIa3GUhdy0GpsNhdW9w2qbh+7xUA+lUMZEU2CsSFGQSbtxQTs1h4/2SydltRdodjYr/O9NLMP8554Ev/30LM499hDcOIZp+L7Z34T//7rn3Hs+d8lrAe++5db0bP+AjhdRvSsW4L+jYvQPCmJyuYJcJY3IUdlGT39BFX+yNK0TBjBh067pBD1ZITKJ1Vjs7oXmYXhJOGtHFepcnzp0CDidGc5q6O0nUSyL1yD8nJ4TZWoaBtVDLgUWO/CBZj74suonzIOMSK7+c8+hcNnHo2BSBfuueJiRB69B5Ma6lW/iGT6l/b1osLjQzXXSrqrzEbT8VznnRRUyeUx0sz2Whx7GQwm1a9Q5vYiz5/b+wdQRvdVSQTWk0yo0Ce7NmQTWpoCGSZYCBCem9R8Ik3VjwQpOhjPVqxajYPSI/jZDX/BV37eCz/5x0v3348xUyZgsG+g5G6h+JZ3r5nq37VjpiMs5YQ4g7zn7e/fiBaKI7WdqmsWKHxY0lMSuvkMLOEtpXssCjW73UtkcqaNPDBlCyBnsPJvdsYXXkdmGPwXUiNhVb63EDRkQiOY+IXjMNLZiUYCqCXzX0fftVdgzctPwbjiDexVV4MElcFBt2sx5FURdXlvL8pbnfBKLjSTnh3PZXce9elqj5Pxag/d2hB9rIwKaCqrxEbC1qFIDBVlfgQsJnIpo4pBacnTySY0yVrwhgdpYQQgcBIyc31UB1LA68Y6wvKuze2oJQprrqjGqqcexx77z8L+B8/CgKNVuaYshZLkkSm5H2syDAdScBvoQrvjSJTmDMlnuUShBCgYLP/JTahYpZusH3wKeZVGsORM9hdRkMNHr+Dkj0SbmkX2iyhumB7oQ8Oee2Ii4fo/L/8f1NXXI0NSu+r6y8gpPahvJvmWBgLVyWRUlQDZftoXjaEnHkcjFb4zMjwtCt2pWP/OWBStaTQBxL4JKfbFY6jiB4k19fPDJO7IeAGxHj8hp4yu6YmmVDleXiuWJMW0ISlRm010e8XuHKkJdaxaixVk/rWybXJTO8bNnKG+oCs/0CWoAL2Mu2YqmimXgiGfhCNDIWWHS/u2jMqVbg96JNeuG3ZVrllXMDpPq9LMdhjsXhhJKXS6e0kqIzqk0IUulmDU8dYbizE8OABDUwMt3Uf0Z0SV2046o6ndkuJet+ZEjaXpZ1KzayC9sZnMNkM2O5UfOvdjw3PhTWZNO9dBotZBf5xhEJdm/CSRWw8DO7kAajwe+l6L0hgztVQ0Uaqc0pSyNTbJjSdkehetTUQoCdsUfdjy+QvUMpv4/mmCEkn9wOKEOx+CJ9wLd4qILD+AYGEYjkKMC0erJAqUngxdM207CurZiF3+EIWjGygwpqm+DUFtAhIMBuX6NApKTySwZeNmGAtFNyoDFslSVEu3qnrL9iGTRb2+UJpbKGi5n9QkxNgoU2Ko0JN3OjNhN5pOTPFie7iQMueutX6U2hclaM9GaC27L6z8YNmymZYmE54bpPV4TcZt01akh0+BikJxi0xBvIkNFNRrSA/2wixV11L2QrbR6HQRvkQn3MY0jPTjujMA3VNV+v6LPHbfo9gCpxPFFmLD6pBr27qZWqDVAJVr49p18JXaDJz0OImsrmYiqWw8ldAqZXlaVnEiZ7EGGqeQRFDSrMPzxuyUoLgsY21GY1uUFhQj7HERqclgp1AyieFYDOU0XbfdpnrapMFRLE12+rmtdji5qF5ekQjMJb0PssjQioiPN2/3uLBu0TLMe+lFIoPgNoSphsdJR5GJroPMvkAt1gll9UyiFF807NaHKuUb3/5WAJl0xvvJE9lq5HJr127AlnUb4fF5GZ+zaoeKZMojsrORVuew2JQCymbxQuFtNCoxLpwq7g3W9ULrTgnKYTQeLrsAY5Kyl5kRsmmYmhIn85f+AB/5To0Md3J5VRlCSu58DS/WQYtxULvscFGbHORY0iApLq6gF7tjZfd4np5k/vPPK/xrejfpkyovhVOIjRQ1WTaI6Z/gkPkdEdjWEj/vqaC2eOpYvGgxIrE8PFwLQYsWehlxdTH6Pzc5mYXrIm0GSlDQtypkjFC9P0yFF6XkOWU7JSiXyTQ6zzdP5orzfUwGpe8qZaRAAS9K5tTJJC6bw6Oqug4KRDRLFlW0SYQm2QqxOBnbpm/9T3EzYPOyZar8bnq/zhzRZO1tpLbbrelDkKOFriw5OIgVS5bAS6DplHF0iiMa6O6MfDar+5eRqKKcedUCrW2dJLPGqBl+LbU8Xa2jxbRTguJJNVv7wXUiGOmC3b55RG2IFt9byKsRBCm6qQp/OcqDNTCbpQ6VVahHBiO67G612Ea1oUBXxNfts6J9+WrVNoxA2TuGfrwjTnxEAekKZZphDQTV7gtxL1a/f9tOjJ0yLIk3tCaTw4olS5djxdLVqCh3wWEq9iyKMluokJIK01QPesnt6XnFkQtF993LmH1HOptTQ9csJrNxpwTFN61WrXQFqAmU6RKPMtGcBRwIWNBL2QfZJC2aY1c94C5UltWgIlAFvzcIl9OjRuhoJUvaulRmm12lbp559FHFGEzaLppsTJcTYpBf8OzTSKfTqm9v7uOPIRaLfmAJZEdhuyZeIRZWP78y50VI+4eT3sBaamU2qPm5mlJE6EWAJB5IL6HoXLFpZ7U0bvH8wXxxt4i2U4LK6npO5ftKu0XEOsLRYWUVwqHUzgW9+LMQXV31jZtVs6VYh51xyuf2qxmuMjVZxoSqvQRbb5v/KPcYMefBB9G3dhlMUrreBd99K8Feal79nR1IjgwhGw2rKcpWu/09TS8fzZosypocdgva31qBl556AdUBKzyWt2dryFpYSlM5i/MuCmrdDCVEq5Rew6sltjZctDBkdkpQlHZYoT+tmM7J0/2lGYdk47KVmplIxtW4AOEOYt5qgpehuGFLLlQgqVR886UL3ToDVnXjlZpd7F4XBjYO4ZF/3Ecf4MOuYENZsv7ymhocc/Z58ND9WRxOTDvkMNUXnv/Y0yiLc9s1+VZtCubuO/+O4ZEUfD6XKvGIBdGWJHenXKBqnTYUxwMJ0RWhJfNFIMWf5xTfUYsroJHPJ3ZOUECPfIDJUDQDIbyy+ITsat+RgIZQeEgxbZUZQGlH3nuaoXRV59gqRGyHsnUZOeA24F+33o6+dUthrq1VHa07B9CovYT2IjCTPwBzsFy5H/lQ7eMAElFA8p3c8CBsfheefvgxPPPw86gqd6iN3DLARMaoyk4OTbXP5bdVHrJqF7yu9iincsrK5vHHSHFDhdEogkrlsl07JShqySLZKCdzH2ShBXIKspPMg6C7DJmrWFWfzHaFvm2P1LshtILjDK6mbUJ8O1Cpjdd+D8JbhnHTtdfyRDfMdvvHc4EqAOoweUkXnHY1vCPW3o7YmlVIdmxWgdxUVg6j4yN8NWtJSNlYhJRDw1q6vN/9+rfw2+khrBa6OU0pnaiWoFu5PxGO3WxUXkb6SmSDg+znEhJsNGj3lOI/ld1cKecyvu9c9pxa8CiV4EaH2aB2mMvoABngUebwwkeiK9PChNwlkjHkbMZtm7jerbR6STMkfm1DjVrJsuQ1BBF15XY8cfM92Hf2bBx26tdR6FxZ3Ge0oxagtMGg+slTHR1Ird+kdmLkeCAVL4ZZIjVTRQWc06bDXE4rk5TVh5Fo+XyZz8R7diCN8OAQfnHJL/i6BCqbgqpaLYlo2YAn6E7aEMRCxNM4HDbVSifCkumasvFb4hN1/u/KWxUKFXazuSZbyMr5z+6URVk1rSeez/9ZkqweCiJBqckWSJmAUu1xIZrJKJM3EwVKrk+4grhG7T3orbgYJinN628nUiVO2UxFARscdgTNwG8uvBgrFs2BuW68Sr/scPCX96ebC899Ff1/vRGZhQtg6O2ChUTZ5nTDEayA1tOD0AP/QPf/XoXwa3NhlG2fH1RdFSFxgWV7jr2QRDISxg+/eyk6V2xCbWNAEf9SnFHJZ1EqiYEJeh3xQjJrUKZrytqJJcXUmG48yJeMoKjQe8vfMrmMdJ8t3ymLyqqhhvkLDAbTflUuyx4bh1Kq+BdJRFFOjWmnlcmwJ5Mhr764RGQgXMpo8L+Db21FQALd9VKLfLEriUzeZFB8bDiRhq8qgIHOYVxy0sn40U034sAjT4Ex3IlUKPSBMx+2pbs8HsQXzkf4Xw/BPXES7C2t1DQrNJuVAgkoZTGOaoRtv5nIbN6ExLy5MNNFqt67eOy9QhLrpybZjOSHQ8P43rd+iOWvL0dLg18Bq+0TFsWsiqb4WjSVUvk+iRZJKrTEsJ5kRiUNKLxfbbteo/FkieqJdOr3AkB2zvWV3EI8m9/fYzUuNBm1ceE0iaolSW5kg8fuQHckghqnVEttCv1FExFqW7nKWOTz2VJis+iTJa5p292gGgPAw2czK6QkG6jL64IY6hrCT48/FWdfsQznXfJj2Lw+ZDu3fLArFKHLdhl+UOU558E6ajQK2y26KkcIsayrVTzIOetA5IeHkIvGVIPke1wor10VBk0FDK5ajx9f+GOsXLIWLY3+YlHyXfFTwJZMSZPfSuFUBmnRxhSClfaD4XROrE4m2b+1NT5ZTaazCnpOz+Yz/2f4EP5o2EFAqk7MS7K3UNiz3GW+P0oTVr168pU+Lgd6YzE1aEoNAOYNxlNJjIQG1YKaVO+Cvi3t5GBQNpZ2MmzbNKPKIVDTVmTWuSAmX00AVXYDbr70CvzglC9i05rVdIVjYSsLFnejvxtoiDBIbG0Tp8BMC8nJTkEqkDr4s55MqeZ9+XeeACM3PKLmvqr9Ue9CmLrU1WiFZrcZm+k+v/u1b2KVCKkp8IFCEndvF0KvGk0LxcIi70741EAqq6yJ5313G5ou4PtmBRBzX6bqfejXjHykFICczKCZolV9qT+eOa4nkUeCAbrGaYWTpHY4RV+M4hc+StCMy1aYgW7VHWsxF7PHktWQMdVEOsqliLvTS5aWLxQzFj67GQGHRf3dSLfUWuPGwgeexncOPAjX/uRCDIfCsDaO40La3gvhZe8wraNQ2iT9HwuD8t2HW3caloCIDPKwuWxAqA93XXENLjjzfAx29qO5OVgcsP+u91UTqGVTnCStyTHFBUoFN59LKaQX5u/7UzL1zCDNlstKOjXZbNR+7zBrl9Gj3Kf9hyTzR87VaCgGTJ/d9O+knr9smIFSZo1Xedw0bZp5QV2QGs0mw3MT6TgGBnrUXCCZkizB10n3IPUX6USSofBFi9qWrlLWJW6jjApgKX0TTT0DtzEWxz+u+T9866ij8dSDdwOBAGzkW5qu75JMhrg62XNrpgtf8eLzuOCcb+L6q29UbrGqtqxk9e9kh8WMjKZms0ulIEKLluyMpNWsBl21IXQlMlKS2shTflZ6mYW3e6rLYviJxYhflcjvh4rK9NEpSvEy6/ySbC38KpLXv7M5Gqmo8/iQcnkwFE+ob4iJkC+laUmFUrZ4JDSgvq/J4/Khvroefpcb4YG40sRii7S+DWzoQGlnuaaGfwzxRoVkeyqCcFI7B97cgF9/4/xfvDZvXsO3zjv3vOqxU+mUh5AeGnpHGWLHtc+gSKrR60aqswN33HY7Hrr1TqQGkhhV51W7G/OlPJ3a7V9SKK3k0EVQup5XQ++lC0tGMKTp+gN0490ER7IF1mo0HL2dgGl3uIJvldxR/frYzQWycbqYVtKOj+YL87qiJIIWu+pO8vACC7JbXX3lASFpJqlqUrIjPhoNodwXUN99u75nC4OpreSvi3umVAmldPX5kmuUfnape/kJNrKMX+YGQt5k6sZXHn9iYM2y5SuPPPyw/znhxBOC/pZxhFghZCiwwo5wr9LnWANeVTFe8PzzuPGPf162/Ln5k2q8BlSQIxXjcGHb9QSdFoKqnJoGrQgu/+SwGBhXi19HIYonXxw2kozC4bEwHEj/okF2vKzeHkiXwPSOG/suKFK/Tlf302gud1VKL37DmVHmSPAuhhhAG+jr41zkKK1J6jICIkbCw6hmAFezGAxaMRDLkA2ZBiZf7aC/I3P/jky7U8r+JnvOiawhSMQ5YjT84Y7bbrvtheefv+jQI4644LDDDi2rVjvhuYKDg9Ts1HvIrF76NgELyS5sbnQtXYD7Hnp4+TPPPHdm7s0VB4yqdFwn4wvy7+Ju8hZu2QCRK35fIxRvKtaeqjxeepM4WmSwvezYpyx6EvKNpfgnscSfd9or76IS2tXUmgoK6WKZrCLtWz4ipp5wGBUyIodXKk2UCQIJ+U6m7sFujGqZQMsqQ/dIsXlTUJLMMHJYjNvc3/adugW92FuYU30XWFXmc/dlGKxzJCrOMWPCsZGRX912003XPfXYY2fvO2vWWTNmzpi6x5QpsNbVF99NBJYp9TkIweWn9K5eiSefeLL/qSef/N+hWPz3ZWHyNI/9Cl222LxL3UVhhLw6VUronaHAYzWraxN26GYcXkM3L3uRQ4nCYqNRO3WXlGx2Yb3ze7SiLWbN+PtURgbvOkhsbeiIpdDisSvfLohINLB7uB/1VfVorKjCpv5uAgszRhiHZO6EzC53mItZ6O2AnFoQiVM2ajSh+/ohSWzSpWrUYr2snHzOCc+oUZFEIvHHRx966I9PPf74QWPGjj1x/PjxR44ZM7pNprB4SYYlKbph46tY/Mbit+a+/PIdPd3dN1e1tMTqCQISsvHNZhv9fmhRShM+frYokoLe2ttAwkfSHyJZHlceQFgGaWVjIG4UU97/s/r1rtfykHzVL+OZ1Il+3lhXNIPuZBoV1DCzVqR/NnKNjd0bURFsRNDtw2Aiofw/tU+NUZNEprFUQxP0JEIWYCGNNRIbqb0LVDkhQ962aTOsVAoZFycuxyY9442N0on7wqaNG19Y9uabMkxktMfj3dPjdlflCvl4f2/v/Ewm81YwGMSoiRORHRlBYs0aSRVVUStGvZ+gxJLdNpPKoMjP6svH1O4Vs5oiLRkIndc5QI5mNpozsXRhtmSP8BkVlDyW0VWdlMsX2kwGfULAZvricDL/Va4qvFxMmyblAAtGCD4C3hgay6vR175GLYC4OBGECMTJWJXO5nu5KEsolC7Sgi1cvi5a02aeNleVD2S0KRfZ2NsLS2ur6q3blqri330+H/yMhRTa2lwutzYSLTbGlDE2ydQvMdUcLTK9dm0RHsvcAl2XzIH01/nejXbLCSQEoitBUQrijssZgwXsuHm9w4k4rT69IlcwnKRp+tpduaif2Ne7cqHW0Vuss5sNj0j/SiRVODFnoQBMdF+6UXXThkb6ETC7UcaYkSpE1HYdEVgsk4smsrnvckHu4wKli7PRi2UE47uQnBolqmkfSG63Cs1sNqtj20zBrcnW4WEUKCyDNNXouiRGD+QhG3j253ESj9N5HXax8mqPDVtCSZ3/1gp6DkGHlcTcWvw+ekgpI/cYY/AJdO/5Xd1/84l/a6hon92sneS2GO6WHReSPR7JZhGi2+iVlE42giafE3ZpvRIExbDPl+1DV3gnFzQtLN9o0P5zC9fH7CJS1VoR9jsFHeXxBI/zeLQybl7XGnRupnAvWjsQf1zQoBQJZRiXNJ46LGr096W83mMFxX8SPVK74Xt4i1yDzP0Mp8XwfaMqVBcJbZxr00M0ZjbmMaEqoGaTR9LZu7K5wmrhJJLvkw1vEr8+rQYxxsMeurvvkUI0v9EV/lN3JLm3nzxxdJkbXsYsuwn/km+OoJx/+0leh2F33XBx9g/+YDZp4xlfHjBoWukLUgyIEikFHAZMb6gQNj+rzGnxyvdt1POodqutk2qM9u5s51NbWAvFnvFGv13guc74eXed11E1utw1VOY03045zaIVncDTln/S12Pa3RpKzZMvrTiFh7DSk+jVDuBy1Iwkki6/3Zqf0RQkP85XmoyGcKnFAomsA292R9RO+q0pp0/6IZOexWLqvDa1fUjikt1kXErM8IRB0x+1m7RYoTQTd7cozi5JZn7++MQfhs+X4HNBff74XFCfC+rzx+eC+vyxM4//J8AAbiEvOxULdF0AAAAASUVORK5CYII="],
        "8": ["[喜欢]", "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGoAAABqCAYAAABUIcSXAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQyIDc5LjE2MDkyNCwgMjAxNy8wNy8xMy0wMTowNjozOSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDoyMkUwQUI2NTk3OTIxMUU4QjlFQUU0ODM1OUI3QzM5MSIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDoyMkUwQUI2Njk3OTIxMUU4QjlFQUU0ODM1OUI3QzM5MSI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjIyRTBBQjYzOTc5MjExRThCOUVBRTQ4MzU5QjdDMzkxIiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjIyRTBBQjY0OTc5MjExRThCOUVBRTQ4MzU5QjdDMzkxIi8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+R9b3SAAAO0pJREFUeNrsfQd0XOWZ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"],
        "9": ["[心碎]", "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGoAAABqCAYAAABUIcSXAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQyIDc5LjE2MDkyNCwgMjAxNy8wNy8xMy0wMTowNjozOSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDoyMzM5MDU5Qzk3OTIxMUU4OUQ1QThCMTA3OTZFMjM3MyIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDoyMzM5MDU5RDk3OTIxMUU4OUQ1QThCMTA3OTZFMjM3MyI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjIzMzkwNTlBOTc5MjExRTg5RDVBOEIxMDc5NkUyMzczIiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjIzMzkwNTlCOTc5MjExRTg5RDVBOEIxMDc5NkUyMzczIi8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+yI6JeQAAOkxJREFUeNrsfQd8VNeV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"],
        "10": ["[疑问]", "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGoAAABqCAYAAABUIcSXAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQyIDc5LjE2MDkyNCwgMjAxNy8wNy8xMy0wMTowNjozOSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDoyNjFFQTMxQzk3OTIxMUU4ODZCQTgwMTgyQkQyODUzMSIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDoyNjFFQTMxRDk3OTIxMUU4ODZCQTgwMTgyQkQyODUzMSI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjI2MUVBMzFBOTc5MjExRTg4NkJBODAxODJCRDI4NTMxIiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjI2MUVBMzFCOTc5MjExRTg4NkJBODAxODJCRDI4NTMxIi8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+5CEbRgAAS/JJREFUeNrsvQecXGXV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"],
        "11": ["[晕]", "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGoAAABqCAYAAABUIcSXAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTQyIDc5LjE2MDkyNCwgMjAxNy8wNy8xMy0wMTowNjozOSAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDoyNEE5MjVBMjk3OTIxMUU4QkQ2N0QxMUVGMDAwQkI4OSIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDoyNEE5MjVBMzk3OTIxMUU4QkQ2N0QxMUVGMDAwQkI4OSI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjI0QTkyNUEwOTc5MjExRThCRDY3RDExRUYwMDBCQjg5IiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjI0QTkyNUExOTc5MjExRThCRDY3RDExRUYwMDBCQjg5Ii8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+aSwaJAAATDdJREFUeNrsvQecXHW5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"]
//      ,
//      "12": ["[调皮]", "data:image/gif;base64,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"],
//      "13": ["[龇牙]", "data:image/gif;base64,R0lGODlhGAAYAPf/AOfGQv/9uf+7Ff/EH//8sqFjCf/ePPnGMf/wXdW9ov7aOMyNWuSXCv/cOv79/P/wX//7mv/7lfv49f/GIf+3EKZaAv//0P/rUP/8oPOoDf/2eP3SM//0bf/tV//9x//iQ//dPf3bPppSA/TDL/uvDPPhz/bm1oVGAf3ZO/vUOf/VMefNVufKTNaxiv/FH/fGK+umFopKA9qOC8t+CMF0N401AF0jANXGvdfRzP/5h82QX//DHdfSzf/oTP/pTdzX05FMAf/mSP/+/t3Y1Orn5Pv6+o5MAv78+//mSePf3P+5Ev/4e//oTf+/GfOxF6hYAPz6+P/LJfnv5LSAR//SLrZ7Kv/1beKyUvrDJsWebe24S/mzE/Du7P/+/Orj3P3467l5DOafEMmphuG/jOfe0PvhpbyCFPfIW//PKqhdCf+5E+WmJuOkFN/a1seCFsuJGt2cGOfAOPy0EOTg3eemFP+6E97Z1eCfJP/9sqpjE/nv5ero5beCRbZ8Nuq4UuGgIf/5iP/SLdnUz7mIU7FzK9WOEbZ5ItylRP/uV/bYmfnEJcyOXf/89/zTN7NvGq5wLNLFuL13FMKKPtiVFejl4vbr3/38/OnIkfnFL7ZzGO+zMLl/E/PesK5mD7NxDf/XNKhXAPry59ixa9+4b7NsCMJ8FPbGWfjKX8CXZ/vILMuLWNzAo/W3KfGrEsivlsyLIv/lR8iQPsByNdmYIf/+z/XBS//3e7iESaxtJffBRemsLd2cH/a8JtO7n82sg///1/W5M/e8N9GsdM6ACNKWM/jbmubi3//3eP/2dOOtHuaqH86EDuzRTvCjC9mkQr1/LvbEL6dgENXEtbd7MvCwH/+2D7uRZbx7DPTy8e2+WP/RLee0J+e2JtjTzv/GH//AGf/LJv/3fP/ePvi3G//xX+zHO//MJufEQOzIO/i4Gv3cP//dPv2xC/2xDP/+x//9/P7ZOPSnDP7IJf7JJvju5fnu5P64E/65E/+4E//pTtW7of60Df60DqxZAP///////yH/C05FVFNDQVBFMi4wAwEAAAAh+QQJCgD/ACwAAAAAGAAYAAAI/wD/CRxIsKDBgwgTIrxhw8aNggwdKvznUJqNU7lYAatlQ1gvGxP/lQmGqREKA/A2pOKVjZNCRmcOhAjCxEcPJB8aqMCiyw/CLqYOoKtRg9kDBIjOEd02j9qVg8WgwarBYkWNcMes1AAQp4Y2F3dGFfyiSd0FQ8hy9IEQAVAVchcyfTKXzhkZgolGBEFkJQcGPAHwYMhhBRGSBmh2wBBVZKCWFD0QHItAwJ0Fd3giHEPQw0CgAVuIURq4BoQPcksgBKD1y0IACEvI+TBAxYW9Nz8G/jHQr7fv38BdKHHDY+CuBk+E6PDnjwZz50cW1DMBageFUjgGztpQQcLy5s/9Rb6XUqJCE36Rsgs89MLTqu/OwUfX04KUgHjPigu8pIybiEXhgffOApWIQEc1MmQxxEChvKKIGUBAAZ58qgBxjRrNTAPJHgSNAUcUmxiRgCxCNCeBPkaAUUc7naDSRmMDORDLJBMkU0AMJ+QYQwFs1EHCMLfwQMRBkhTixA4uTODCDk0IUA0DeQyCQxIJ+eLIMmG0ooQcGTAwwyOuCGLMRF6IwQchaUSDyxTW4GAHNiH9YwkXSQyBQzc/zDGkQQEBACH5BAUKAP8ALAAAAAAYABgAAAj/AP8JHEiwoMGD/27YsHGjoEKGCAcylGbjVC5WwGrZENbLRsSBZYJhaoTCgIINqXhl4/SR0ZkDIZAwycckyIcGKrDo8oOwi6kD5WrUYEYOAaJzQrfJo3blYDFosGqwWFHDlgYrNQDEqRFowp1RBb9oCnHBEDJAfSBEyFGF3IVMn8CNc0aGYKIRSBBxyIEBTwA8GHJY6RCkAZodMEQVGaglRQ8ExyIQ8EDLHZ4IxxD0AKHNxRZilAauWeeD3BIIAWj9shAAgi1yPsRR8XbvzY+Bf0D02827t28X+NzwGLirwRMhOvz5o6GcuYMFekyA2kGhFI6BszZUkJB8eXN/7xZIvylR4du+SNcFHnrhaVV35t6f02tBSkCGZ8MFXlLGTcSi7/EtUIkIdFQjQxZDDBTKK4qYAQQU3sWnChDX1NHMNJDsQdAYcESxiREJyCLEchLoYwQYdbDTCSptLDaQA7FMMkEyBcRwwo0xFMCGGiQMcwsPRBwkSSFO7ODCBAPs0IQA1TCQxyA4JBGRL44sE0YrSsiRAQMzPOKKIMZ89I8XYvBBSBrR4DKFNTjYgY2YAlnCRRJD4NDND3MECeeeBQUEADs="],
//      "14": ["[微笑]", "data:image/gif;base64,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"],
//      "15": ["[难过]", "data:image/gif;base64,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"],
//      "16": ["[酷]", "data:image/gif;base64,R0lGODlhGAAYAPf/ANWiOtWkVP60Dv/89v/cOvjGMP+5E//XNf/1beXEav/wXdnUz//VMbhrEv/wX2loS/+7FDIsDf/GIOSXCv/aOf/DHf/tV//pT//9sf/ePf/AGv/FIP/+0P/6lf/KJf/4e//5iP/3eP/3d//dPf/EH//9uQ4LA9ShMtSfLP7hRf/gQf3dQ/7dP97bgv/TLv/PK//LKdXHSvfGK/y1Ef2xDPGsEtSmRvCjC9ilGdqOC5+SM7+HEnJuNmZjLmZaGxgUBdfRzP/fRf/lR6WclKpSBr5pCdfSzf/sU//RLf/qUP/mSL2Xdern5OPf3NzX0/////v6+t3Y1I5lQf/iQv/LJv/oTP/pTf/oTf+3EP/xX//uV8t+CPe8N9LFuFdXR65wLPncnOG/jOrj3NOYEPWyEs2sg+fe0P+/GcKKPlpZNv/ePr13FKxtJfbEL/XBS5SMhffIW8iQPvry50g4CYV+ePW3KeGgIe24SyQiDLiESfzw1suJGqdgEMyLIu7CUntvI86ACP/7mzkyDqhdCf/hQrSAR/jbmrd7MvPesOC0N++zMPjKX/fKZ/3XOcivltiVFf/TMOnIkfOpDYczAPTy8bmIU5OQTruRZcCXZ/Du7PbGWczHbdmYIZyTOv38/P79/Pi3GvnEJfvhpfzlr65mD//MJ+afELFzK+HZZuWmJvvLMPrNN/zPNP346/C/JMJ8FP/+x+ro5cWebdWOEfbYmfHIWG9tRv//197Z1d/a1v7IJXVwMmZeIPfBReCfJMeCFvuvDP7nT+jl4t2cH/3fRMmphu65Nu3HU/W5M/SnC/CwH86EDr1/Lv/2dGZgJrNvGtixa+Tg3ebi3//+/PzfQf/qUY13Hf/rUfPRNtmkQtylRKpjE9KWM7eCRbVpHeC3PTw4Nfz3k3p0bv/rU//8mtejTdKUINKYJeaqH+XFW+aqIP/3e/OyGO7YbeCoJOC7T7hsGO7Zgfn4woRzIMaHK92bGN2cGMaJNf/XNP/7n9ahQbVpGf/RLv/SLQAAAP///yH/C05FVFNDQVBFMi4wAwEAAAAh+QQFFAD/ACwAAAAAGAAYAAAI/wD/CRxIsKDBgwgTKhwwalGvOsjcMAKjR+FAUVxWrUhBiEUjVgVqIVoIp0CwK1esWFFCiAADVcb8IJymqcC4JFoUZFGgJYmQDAx0KTt20FCbC4lOIBDx4R2CLEfOueO3wVeCgq0UEUuSBZ6NDuTugWh2ogEhfFRAZTNDkJYJf3h2bQpAJIG3eEQAxPgTwZ+/HdCgDLzzw6/fB3RhEdHHw7DfMdyEDUwlyPASKV5ucSiRxp+UJYbJ7HEy0M48y08c+30C2t8cA7+MDByGra/jBxhsqf6BA8srIAM54aOmw1kPSy0CdQCBqhMvH9ZcnRGwBrhAbTIICDmSBUGIdSKeHrYRQgCJBAjJmMkWGEkdgwxCruXc2fMnAyoasOSQFWWgnD6hUEDAFEpUYcUVSgThUikVGHDDIV3EQlAY9VBxAD4UjKBGEUUEgQQVFUBAAymY5CLYQJ/E8YgEkLyARD9FTBKEBBoYAAwgeRjBxEFozMIODCRsEEQQZ0CAxQTbVAJEEwmV8cwyptRgwAySTLDFF44sII1FYhTTzSmD8MFGIZcAgQslFgnkSSZNRAHEAk5Es2OadBYUEAAh+QQFFAD/ACwDAAMAEQARAAAIhAD/CRwoUAXBgwQvVPm3cArCgYlQDPxmodo/c+4QHmkAQCC9fw5QNCCABGGzf0TS7WtH5F8Shw/zlSOCgQgABA+HgBMoj8M/DOEGihsy8M0QojmHvCFIBynCIXQeSp1K9Z+Ghx9CHOwnEN1UiyP+eZh6oUgRqfYGEqj60AXbgRsGXj0YEAA7"],
//      "17": ["[冷汗]", "data:image/gif;base64,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"],
//      "18": ["[抓狂]", "data:image/gif;base64,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"],
//      "19": ["[吐]", "data:image/gif;base64,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"],
//      "20": ["[偷笑]", "data:image/gif;base64,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"],
//      "21": ["[可爱]", "data:image/gif;base64,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"],
//      "22": ["[白眼]", "data:image/gif;base64,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"],
//      "23": ["[傲慢]", "data:image/gif;base64,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"],
//      "24": ["[饿]", "data:image/gif;base64,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"],
//      "25": ["[困]", "data:image/gif;base64,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"],
//      "26": ["[惊恐]", "data:image/gif;base64,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"],
//      "27": ["[流汗]", "data:image/gif;base64,R0lGODlhGAAYAPf/ANPFuahcCfi4KP7lS/a5KOltA4lJDrWBRv/8i/7pUfncm//2asqplf/89rZ4Uv3XPf3ZPvHu7OybFf3cQtulQ9mXIaJCBfzQNsmGGPaFANqGD//3bv/+z//+xbhXBP3SONjTzvjOnP/+q/eRB61wK/u+JtF4Cv/yX/mlFf/9kP7gRfV6AJ07APaNBchjBfTp4rJ8S/vBKf7lTtRqB//+s8dgBfaBAPW1KNxpA/mpGPq7KMOXfPvDKveWC+PGtKlbI/ifEMFcAv3eRumCAa9lD/mqGP7hSvidD69nPPrz5+W2Mf7cQ//1ZuOUFf/yYPfLZ/vPN/bGWPXCS9SADffCRe7DUvnEMfOdEufFQfW6M8J8FPq3Jcmqhu+0MPKlGeSmIuGhIfaKA8yMIrmJU7FzKspvB7d8Mvu7JriFSbl0SrdMAOuzcIczAP/rU10jAP/6e/zKMNfRzPu8Jfq0H/mvHP/9luDJf+Pf3NfSzd3Y1Orn5Pv6+tzX09qymvPesPzw1siQPv346/fIW+uFA/W3J/HIWN2cH//+/O3HU97Z1c2sg/79/OTg3enIkfe9KPzlr+65NuDDTIpMF+fQWOWmJvvhpcKKPruRZYI6A+rj3O2FAqxtJfq/K+DBRvbYmfO0JapjE65sFvayI+24S/zROvjKX///1/e8N/38/PedELNvGvmvHubi3+ro5fTy8fRxAOp/AfeUCfzGL8iwlv3bQvmjF+HAjN/a1vzKMvigEeuHBvaWDPvQOt94BMWebdq3oqdgD/FwAb13FOfe0OCgJJpGBOXDavGPCvqwHPzJMOjl4vR1AP7rU9KWM9ixa+yIB+aHDr2ALvCCBMCYZ/eVCveeD//pUfvGL/vJMPiWCvmvG/Ln4b1/LvmrGfmwG6dgELd2S/qvHMCXZ/3dQvBvAfzJMvmjGOG/jPvPOuB4BP7qU//qU7BcEqZPC+2SDsqfgN6JEcOIZMFmCeiVFLNtQvjx7duved20gdd1B9t1BMalkNGiXr13L7RqK////////yH/C05FVFNDQVBFMi4wAwEAAAAh+QQFFAD/ACwAAAAAGAAYAAAI/wD/CRxIsKDBgwgTKmzwqBQVQlmkPFHwR+HASqesoCNV7honATcK+VkoiICRJQ8gPPiQrIQ3UZCqIDwUhYCMJQOsrUswYNyFGOFSSUB0UAEhFSraOFmwYYETdSoulCjSowK/egQDdYESik2kNwhSIHjDpM2EZHOA4LP3jqCnTw8GdGLDxg4NEXXenBjwQc6VICH6RRg4ytEEawsQ2GEDo4MIBAsSKGHzY02IezsGUuIBgdmGFDQ4mOJAI8Ukul9MWLaXWSCYGA8QIxDRgYNjEmywPIgxb0YQIr8GGjrzYcCJN3VE3M27ty+KESu0xBlYYQ6cCQbcaJc0Nrt2TEBaLMYTNl0gBS8lLiRd2vREGyFSc/TIUIAbnoGNmmiLcWHCgATM8ORTDMhU08IKM/iSx0BJiHGFDiVg80FKK8FRAjJAjGBDAWYA0ApB52hQzRZznBEDDzHIMUcO1YyQwStEiHPLHgQtAsgU2ayCQg7d5IACED20YAM5LqCBhx4HWWKCLuaMMMIgmoSRwQo4gDJGHHckpIgqZaQzhD/+wIJDDSTMAgIrFmXCxQFkBPDNJgdcEkcirlgkECoR3JFHHCDwwQiSdgZaUEAAIfkEBQgA/wAsEQAGAAYABgAACCgA/1H7Z2Hbv3/VXPwC9++MBBb+WBhE4sOfDyT/PFS8+A+eBxbt9AUEACH5BAkUAP8ALAMAAgAUABQAAAiNAP8JHCiQAMGDCAXy+odLVsKHB+H8owNR4DiBbawRpJhQiMATA0+0GZhjV0WE4yQCOclSIK2KGv+x6POvjwOMLFn4++ePRUuBOv31lNOyhAcf/nwgschSgxoW7fQdBElQxUFy0QRyvGgt48YjECUipJMLoa1/YP8RPZhW4LSDPSoGq1jrJ8JedhMCqxgQACH5BAUUAP8ALAAAAAAYABgAAAj/AP8JHEiwoMGDCBMqbPCoFBUCWaQ8UfBH4cBKp6zwIoVLFicBNwr5WSiIgJElDyA8+ACnBB1RkKogPBSFgIxxA6y1sTZg3IUYdFJJQHRQASEhKtqcWLBhwYk2Ki6UyLGLmDGDXaCEYhPpDYIUCN4waTMOzhwgxygMK/jpwYBObNjYoSGizpsTAz7IQaHLwr49BB3RsrYAgR02MDqIQLDAmhI2xVz8YqeMIA8IbTakoMHBFAcaKSbF/SKBhT8WAAjGeEAYgYgOHBSTYIPlQQx5ffz1cUBQzocBJ97UEUHXLl69pv2dJjgHzjgDbqJLCgs9Oqbkywd6KXEh6dKmT6NOoPXgw58PJASb0IlxAadOnj6BHsmnhsUPBgTFXNFRAs6HlCu1REcuI9hQgBkAtEKQLRocscUccsTAQwxyzJHDESNk8AoR09wC2ECLADJFD6ugkEM3OaAARA8t2BCMC2jgocdBlpigSy0jjDCIJmFksAIOoIwRxx0JKaJKGb0MoRwsONRAwiwgsGJRJlwcQEYAwGxywCVxJOKKRWCGKeZAAQEAIfkECRQA/wAsEQAIAAYADAAACEgA//0bAc2ZwH+xpDUT2I2aDQx8/uV4ZkFLnH8oXPxqd9Edi38sABQJ0udfHwcg/an8yEKlv49qfPjzkeafh3Ys6PkQiIeRwIAAIfkECRQA/wAsAAAAABgAGAAACIoA/wkcSLCgwYMIEypcyLChw4cQI0qcOFEOihHQnO2RyKZYLGnNlEn80o2aDQx8JMbIEetkHIkWR6zQ8jIiJiAtlgmrCTFHjwwFouGReKTFihm+8kgcYaOAGQCtJGZ4BcoCgI0Rg7n48cvBRFAHWPhj8WKigz7++sSbKNbfWLZu30pM48OfjzT/AgIAIfkEBRQA/wAsAAAAABgAGAAACP8A/wkcSLCgwYMIEyps8KgUFQJZpDxR8EfhwEqnrPAihUsWJwE3CvlZKIiAkSUPIDz4AKcEHVGQqiA8FIWAjHEDrLWxNmDchRh0UklAdFABISEq2pxYsGHBiTYqLpTIsYuYsYKBukAJxSbSGwQpELxh0mYcnDlAjlEYRtDTpwcDOrFhY4eGiDpvTgz4IAfFCGjO9gwc5YiWtQUI7LCB0UEEggXWlLApFktaM2UDKfGA0GZDChocTHGgkWLS3C/dqNnAwGcgmBgPDiMQ0YFDYxJssDyIkSPW6jgDDcn5MODEmzoi7OLVy9fvCi3ABVaYA2ecATfYJYm9jh0TkBbLhEW8/0fBS4kLSZc2fRp1ao8MBaLhGdioCZ0YF3Dq5OkT6JEWK8zgSx4DJSHGFTqUAMcHKa3UEh25jGBDAWYA0ApBtmhwxBZzyBEDDzHIMUcOR4yQwStETHOLYAMtAsgUPayCQg7d5IACED20YEMwLqCBhx4HWWKCLrWMMMIgmoSRwQo4gDJGHHckpIgqZfQyhD/+wIJDDSTMAgIrFmXCxQFkBADMJgdcEkcirlgkECoR3JFHHCDwwQiQbuZZUEAAOw=="],
//      "28": ["[憨笑]", "data:image/gif;base64,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"],
//      "29": ["[大兵]", "data:image/gif;base64,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"],
//      "30": ["[奋斗]", "data:image/gif;base64,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"],
//      "31": ["[咒骂]", "data:image/gif;base64,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"],
//      "32": ["[疑问]", "data:image/gif;base64,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"],
//      "33": ["[嘘]", "data:image/gif;base64,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"],
//      "34": ["[晕]", "data:image/gif;base64,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"]
// 
    };
    //工具类
    var tool = new function() {

            //格式化时间戳
            //format格式如下：
            //yyyy-MM-dd hh:mm:ss 年月日时分秒(默认格式)
            //yyyy-MM-dd 年月日
            //hh:mm:ss 时分秒
            this.formatTimeStamp = function(timestamp, format) {
                if (!timestamp) {
                    return 0;
                }
                var formatTime;
                format = format || 'yyyy-MM-dd hh:mm:ss';
                var date = new Date(timestamp * 1000);
                var o = {
                    "M+": date.getMonth() + 1, //月份
                    "d+": date.getDate(), //日
                    "h+": date.getHours(), //小时
                    "m+": date.getMinutes(), //分
                    "s+": date.getSeconds() //秒
                };
                if (/(y+)/.test(format)) {
                    formatTime = format.replace(RegExp.$1, (date.getFullYear() + "").substr(4 - RegExp.$1.length));
                } else {
                    formatTime = format;
                }
                for (var k in o) {
                    if (new RegExp("(" + k + ")").test(formatTime))
                        formatTime = formatTime.replace(RegExp.$1, (RegExp.$1.length == 1) ? (o[k]) : (("00" + o[k]).substr(("" + o[k]).length)));
                }
                return formatTime;
            };

            //根据群类型英文名转换成中文名
            this.groupTypeEn2Ch = function(type_en) {
                var type_ch = null;
                switch (type_en) {
                    case 'Public':
                        type_ch = '公开群';
                        break;
                    case 'ChatRoom':
                        type_ch = '聊天室';
                        break;
                    case 'Private':
                        type_ch = '私有群'; //即讨论组
                        break;
                    case 'AVChatRoom':
                        type_ch = '直播聊天室';
                        break;
                    default:
                        type_ch = type_en;
                        break;
                }
                return type_ch;
            };
            //根据群类型中文名转换成英文名
            this.groupTypeCh2En = function(type_ch) {
                var type_en = null;
                switch (type_ch) {
                    case '公开群':
                        type_en = 'Public';
                        break;
                    case '聊天室':
                        type_en = 'ChatRoom';
                        break;
                    case '私有群': //即讨论组
                        type_en = 'Private';
                        break;
                    case '直播聊天室':
                        type_en = 'AVChatRoom';
                        break;
                    default:
                        type_en = type_ch;
                        break;
                }
                return type_en;
            };
            //根据群身份英文名转换成群身份中文名
            this.groupRoleEn2Ch = function(role_en) {
                var role_ch = null;
                switch (role_en) {
                    case 'Member':
                        role_ch = '成员';
                        break;
                    case 'Admin':
                        role_ch = '管理员';
                        break;
                    case 'Owner':
                        role_ch = '群主';
                        break;
                    default:
                        role_ch = role_en;
                        break;
                }
                return role_ch;
            };
            //根据群身份中文名转换成群身份英文名
            this.groupRoleCh2En = function(role_ch) {
                var role_en = null;
                switch (role_ch) {
                    case '成员':
                        role_en = 'Member';
                        break;
                    case '管理员':
                        role_en = 'Admin';
                        break;
                    case '群主':
                        role_en = 'Owner';
                        break;
                    default:
                        role_en = role_ch;
                        break;
                }
                return role_en;
            };
            //根据群消息提示类型英文转换中文
            this.groupMsgFlagEn2Ch = function(msg_flag_en) {
                var msg_flag_ch = null;
                switch (msg_flag_en) {
                    case 'AcceptAndNotify':
                        msg_flag_ch = '接收并提示';
                        break;
                    case 'AcceptNotNotify':
                        msg_flag_ch = '接收不提示';
                        break;
                    case 'Discard':
                        msg_flag_ch = '屏蔽';
                        break;
                    default:
                        msg_flag_ch = msg_flag_en;
                        break;
                }
                return msg_flag_ch;
            };
            //根据群消息提示类型中文名转换英文名
            this.groupMsgFlagCh2En = function(msg_flag_ch) {
                var msg_flag_en = null;
                switch (msg_flag_ch) {
                    case '接收并提示':
                        msg_flag_en = 'AcceptAndNotify';
                        break;
                    case '接收不提示':
                        msg_flag_en = 'AcceptNotNotify';
                        break;
                    case '屏蔽':
                        msg_flag_en = 'Discard';
                        break;
                    default:
                        msg_flag_en = msg_flag_ch;
                        break;
                }
                return msg_flag_en;
            };
            //将空格和换行符转换成HTML标签
            this.formatText2Html = function(text) {
                var html = text;
                if (html) {
                    html = this.xssFilter(html); //用户昵称或群名称等字段会出现脚本字符串
                    html = html.replace(/ /g, "&nbsp;");
                    html = html.replace(/\n/g, "<br/>");
                }
                return html;
            };
            //将HTML标签转换成空格和换行符
            this.formatHtml2Text = function(html) {
                var text = html;
                if (text) {
                    text = text.replace(/&nbsp;/g, " ");
                    text = text.replace(/<br\/>/g, "\n");
                }
                return text;
            };
            //获取字符串(UTF-8编码)所占字节数
            //参考：http://zh.wikipedia.org/zh-cn/UTF-8
            this.getStrBytes = function(str) {
                if (str == null || str === undefined) return 0;
                if (typeof str != "string") {
                    return 0;
                }
                var total = 0,
                    charCode, i, len;
                for (i = 0, len = str.length; i < len; i++) {
                    charCode = str.charCodeAt(i);
                    if (charCode <= 0x007f) {
                        total += 1; //字符代码在000000 – 00007F之间的，用一个字节编码
                    } else if (charCode <= 0x07ff) {
                        total += 2; //000080 – 0007FF之间的字符用两个字节
                    } else if (charCode <= 0xffff) {
                        total += 3; //000800 – 00D7FF 和 00E000 – 00FFFF之间的用三个字节，注: Unicode在范围 D800-DFFF 中不存在任何字符
                    } else {
                        total += 4; //010000 – 10FFFF之间的用4个字节
                    }
                }
                return total;
            };


            //防止XSS攻击
            this.xssFilter = function(val) {
                val = val.toString();
                val = val.replace(/[<]/g, "&lt;");
                val = val.replace(/[>]/g, "&gt;");
                val = val.replace(/"/g, "&quot;");
                //val = val.replace(/'/g, "&#039;");
                return val;
            };

            //去掉头尾空白符
            this.trimStr = function(str) {
                if (!str) return '';
                str = str.toString();
                return str.replace(/(^\s*)|(\s*$)/g, "");
            };
            //判断是否为8位整数
            this.validNumber = function(str) {
                str = str.toString();
                return str.match(/(^\d{1,8}$)/g);
            };
            this.getReturnError = function(errorInfo, errorCode) {
                if (!errorCode) {
                    errorCode = -100;
                }
                var error = {
                    'ActionStatus': ACTION_STATUS.FAIL,
                    'ErrorCode': errorCode,
                    'ErrorInfo': errorInfo + "[" + errorCode + "]"
                };
                return error;
            };
            //设置cookie
            //name 名字
            //value 值
            //expires 有效期(单位：秒)
            //path
            //domain 作用域
            this.setCookie = function(name, value, expires, path, domain) {
                var exp = new Date();
                exp.setTime(exp.getTime() + expires * 1000);
                document.cookie = name + "=" + escape(value) + ";expires=" + exp.toGMTString();
            };
            //获取cookie
            this.getCookie = function(name) {
                var result = document.cookie.match(new RegExp("(^| )" + name + "=([^;]*)(;|$)"));
                if (result != null) {
                    return unescape(result[2]);
                }
                return null;
            };
            //删除cookie
            this.delCookie = function(name) {
                var exp = new Date();
                exp.setTime(exp.getTime() - 1);
                var value = this.getCookie(name);
                if (value != null)
                    document.cookie = name + "=" + escape(value) + ";expires=" + exp.toGMTString();
            };
            //根据名字获取url参数值
            this.getQueryString = function(name) {
                var reg = new RegExp("(^|&)" + name + "=([^&]*)(&|$)", "i");
                var r = window.location.search.substr(1).match(reg);
                if (r != null) return unescape(r[2]);
                return null;
            };
            //判断IE版本号，ver表示版本号
            this.isIE = function(ver) {
                var b = document.createElement('b')
                b.innerHTML = '<!--[if IE ' + ver + ']><i></i><![endif]-->'
                return b.getElementsByTagName('i').length === 1;
            };
            //判断浏览器版本
            this.getBrowserInfo = function() {
                var Sys = {};
                var ua = navigator.userAgent.toLowerCase();
                log.info('navigator.userAgent=' + ua);
                var s;
                (s = ua.match(/msie ([\d.]+)/)) ? Sys.ie = s[1] :
                    (s = ua.match(/firefox\/([\d.]+)/)) ? Sys.firefox = s[1] :
                    (s = ua.match(/chrome\/([\d.]+)/)) ? Sys.chrome = s[1] :
                    (s = ua.match(/opera.([\d.]+)/)) ? Sys.opera = s[1] :
                    (s = ua.match(/version\/([\d.]+).*safari/)) ? Sys.safari = s[1] : 0;
                if (Sys.ie) { //Js判断为IE浏览器
                    return {
                        'type': 'ie',
                        'ver': Sys.ie
                    };
                }
                if (Sys.firefox) { //Js判断为火狐(firefox)浏览器
                    return {
                        'type': 'firefox',
                        'ver': Sys.firefox
                    };
                }
                if (Sys.chrome) { //Js判断为谷歌chrome浏览器
                    return {
                        'type': 'chrome',
                        'ver': Sys.chrome
                    };
                }
                if (Sys.opera) { //Js判断为opera浏览器
                    return {
                        'type': 'opera',
                        'ver': Sys.opera
                    };
                }
                if (Sys.safari) { //Js判断为苹果safari浏览器
                    return {
                        'type': 'safari',
                        'ver': Sys.safari
                    };
                }
                return {
                    'type': 'unknow',
                    'ver': -1
                };
            };

        };

    //日志对象
    var log = new function() {

            var on = false;//打印日志

            this.setOn = function(onFlag) {
                on = onFlag;
            };

            this.getOn = function() {
                return on;
            };

            this.error = function(logStr) {
                try {
                    on && console.error(logStr);
                } catch (e) {}
            };
            this.warn = function(logStr) {
                try {
                    on && console.warn(logStr);
                } catch (e) {}
            };
            this.info = function(logStr) {
                try {
                    on && console.info(logStr);
                } catch (e) {}
            };
            this.debug = function(logStr) {
                try {
                    on && console.debug(logStr);
                } catch (e) {}
            };
        };
    //获取unix时间戳
    var unixtime = function(d) {
        if (!d) d = new Date();
        return Math.round(d.getTime() / 1000);
    };
    //时间戳转日期
    var fromunixtime = function(t) {
        return new Date(t * 1000);
    };
    //获取下一个消息序号
    var nextSeq = function() {
        if (curSeq) {
            curSeq = curSeq + 1;
        } else {
            curSeq = Math.round(Math.random() * 10000000);
        }
        return curSeq;
    };
    //产生随机数
    var createRandom = function() {
        return Math.round(Math.random() * 4294967296);
    };

    //获取ajax请求对象
    var getXmlHttp = function() {
        var xmlhttp = null;
        if (window.XMLHttpRequest) {
            xmlhttp = new XMLHttpRequest();
        } else {
            try {
                xmlhttp = new ActiveXObject("Msxml2.XMLHTTP");
            } catch (e) {
                try {
                    xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
                } catch (e) {
                    return null;
                }
            }
        }
        return xmlhttp;
    }
    //发起ajax请求
    var ajaxRequest = function(meth, url, req, timeout, isLongPolling, cbOk, cbErr) {

        var xmlHttpObj = getXmlHttp();

        var error, errInfo;
        if (!xmlHttpObj) {
            errInfo = "创建请求失败";
            var error = tool.getReturnError(errInfo, -1);
            log.error(errInfo);
            if (cbErr) cbErr(error);
            return;
        }
        //保存ajax请求对象
        xmlHttpObjSeq++;
        xmlHttpObjMap[xmlHttpObjSeq] = xmlHttpObj;

        xmlHttpObj.open(meth, url, true);
        xmlHttpObj.onreadystatechange = function() {
            if (xmlHttpObj.readyState == 4) {
                xmlHttpObjMap[xmlHttpObjSeq] = null; //清空
                if (xmlHttpObj.status == 200) {
                    if (cbOk) cbOk(xmlHttpObj.responseText);
                    xmlHttpObj = null;
                    curLongPollingRetErrorCount = curBigGroupLongPollingRetErrorCount = 0;
                } else {
                    xmlHttpObj = null;
                    //避免刷新的时候，由于abord ajax引起的错误回调
                    setTimeout(function() {
                        var errInfo = "请求服务器失败,请检查你的网络是否正常";
                        var error = tool.getReturnError(errInfo, -2);
                        //if (!isLongPolling && cbErr) cbErr(error);
                        if (cbErr) cbErr(error);
                    }, 16);
                }
            }
        };
        xmlHttpObj.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
        //设置超时时间
        if (!timeout) {
            timeout = ajaxDefaultTimeOut; //设置ajax默认超时时间
        }
        if (timeout) {
            xmlHttpObj.timeout = timeout;
            xmlHttpObj.ontimeout = function(event) {
                xmlHttpObj = null;
                //var errInfo = "请求服务器超时";
                //var error = tool.getReturnError(errInfo, -3);
                //if (cbErr) cbErr(error);
            };
        }
        //
        xmlHttpObj.send(req);
    }
    //发起ajax请求（json格式数据）
    var ajaxRequestJson = function(meth, url, req, timeout, isLongPolling, cbOk, cbErr) {
        ajaxRequest(meth, url, JSON.stringify(req), timeout, isLongPolling, function(resp) {
            var json = null;
            //if (resp) eval('json=('+resp+');');//将返回的json字符串转换成json对象
            //if (resp) json=eval('('+resp+')');//将返回的json字符串转换成json对象
            if (resp) json = JSON.parse(resp); //将返回的json字符串转换成json对象
            if (cbOk) cbOk(json);
        }, cbErr);
    }
    //判断用户是否已登录
    var isLogin = function() {
        return ctx.sdkAppID && ctx.identifier;
    };
    //检查是否登录
    var checkLogin = function(cbErr, isNeedCallBack) {
        if (!isLogin()) {
            if (isNeedCallBack) {
                var errInfo = "请登录";
                var error = tool.getReturnError(errInfo, -4);

                if (cbErr) cbErr(error);
            }
            return false;
        }
        return true;
    };

    //检查是否访问正式环境
    var isAccessFormalEnv = function() {
        return isAccessFormaEnvironment;
    };

    //根据不同的服务名和命令，获取对应的接口地址
    var getApiUrl = function(srvName, cmd, cbOk, cbErr) {
        var srvHost = SRV_HOST;
        if (isAccessFormalEnv()) {
            srvHost = SRV_HOST.FORMAL.COMMON;
        } else {
            srvHost = SRV_HOST.TEST.COMMON;
        }

        //if (srvName == SRV_NAME.RECENT_CONTACT) {
        //    srvHost = SRV_HOST.TEST.COMMON;
        //}

        if (srvName == SRV_NAME.PIC) {
            if (isAccessFormalEnv()) {
                srvHost = SRV_HOST.FORMAL.PIC;
            } else {
                srvHost = SRV_HOST.TEST.PIC;
            }
        }

        var url = srvHost + '/' + SRV_NAME_VER[srvName] + '/' + srvName + '/' + cmd + '?websdkappid=' + SDK.APPID + "&v=" + SDK.VERSION + "&platform=" + SDK.PLAATFORM;;

        if (isLogin()) {
            if (cmd == 'login') {
                url += '&identifier=' + encodeURIComponent(ctx.identifier) + '&usersig=' + ctx.userSig;
            } else {
                if (ctx.tinyid && ctx.a2) {
                    url += '&tinyid=' + ctx.tinyid + '&a2=' + ctx.a2;
                } else {
                    if (cbErr) {
                        log.error("tinyid或a2为空[" + srvName + "][" + cmd + "]");
                        cbErr(tool.getReturnError("tinyid或a2为空[" + srvName + "][" + cmd + "]", -5));
                        
                        return false;
                    }
                }
            }
            url += '&contenttype=' + ctx.contentType;
        }
        url += '&sdkappid=' + ctx.sdkAppID + '&apn=' + ctx.apn + '&reqtime=' + unixtime() + '&accounttype=' + ctx.accountType;
        return url;
    };

    //获取语音下载url
    var getSoundDownUrl = function(uuid, senderId) {
        var soundUrl = null;
        if (authkey && ipList[0]) {
            soundUrl = "http://" + ipList[0] + "/asn.com/stddownload_common_file?authkey=" + authkey + "&bid=" + DOWNLOAD_FILE.BUSSINESS_ID + "&subbid=" + ctx.sdkAppID + "&fileid=" + uuid + "&filetype=" + DOWNLOAD_FILE_TYPE.SOUND + "&openid=" + senderId + "&ver=0";
        } else {
            log.error("拼接语音下载url不报错：ip或者authkey为空");
        }
        return soundUrl;
    };

    //获取文件下载地址
    var getFileDownUrl = function(uuid, senderId, fileName) {
        var fileUrl = null;
        if (authkey && ipList[0]) {
            fileUrl = "http://" + ipList[0] + "/asn.com/stddownload_common_file?authkey=" + authkey + "&bid=" + DOWNLOAD_FILE.BUSSINESS_ID + "&subbid=" + ctx.sdkAppID + "&fileid=" + uuid + "&filetype=" + DOWNLOAD_FILE_TYPE.FILE + "&openid=" + senderId + "&ver=0&filename=" + encodeURIComponent(fileName);
        } else {
            log.error("拼接文件下载url不报错：ip或者authkey为空");
        }
        Resources.downloadMap["uuid_" + uuid] = fileUrl;
        return fileUrl;
    };

    //获取文件下载地址
    var getFileDownUrlV2 = function(uuid, senderId, fileName, downFlag, receiverId, busiId, type) {
        var options = {
            "From_Account": senderId, //"identifer_0",       // 类型: String, 发送者tinyid
            "To_Account": receiverId, //"identifer_1",         // 类型: String, 接收者tinyid
            "os_platform": 10, // 类型: Number, 终端的类型 1(android) 2(ios) 3(windows) 10(others...)
            "Timestamp": unixtime().toString(), // 类型: Number, 时间戳
            "Random": createRandom().toString(), // 类型: Number, 随机值
            "request_info": [ // 类型: Array
                {
                    "busi_id": busiId, // 类型: Number, 群(1) C2C(2) 其他请联系sdk开发者分配
                    "download_flag": downFlag, // 类型: Number, 申请下载地址标识  0(申请架平下载地址)  1(申请COS平台下载地址)  2(不需要申请, 直接拿url下载(这里应该不会为2))
                    "type": type, // 类型: Number, 0(短视频缩略图), 1(文件), 2(短视频), 3(ptt), 其他待分配
                    "uuid": uuid, // 类型: Number, 唯一标识一个文件的uuid
                    "version": VERSION_INFO.SERVER_VERSION, // 类型: Number, 架平server版本
                    "auth_key": authkey, // 类型: String, 认证签名
                    "ip": ipList[0] // 类型: Number, 架平IP
                }
            ]
        };
        //获取下载地址
        proto_applyDownload(options, function(resp) {
            if (resp.error_code == 0 && resp.response_info) {
                Resources.downloadMap["uuid_" + options.uuid] = resp.response_info.url;
            }
            if (onAppliedDownloadUrl) {
                onAppliedDownloadUrl({
                    uuid: options.uuid,
                    url: resp.response_info.url,
                    maps: Resources.downloadMap
                });
            }
        }, function(resp) {
            log.error("获取下载地址失败", options.uuid)
        });
    };


    //重置ajax请求
    var clearXmlHttpObjMap = function() {
        //遍历xmlHttpObjMap{}
        for (var seq in xmlHttpObjMap) {
            var xmlHttpObj = xmlHttpObjMap[seq];
            if (xmlHttpObj) {
                xmlHttpObj.abort(); //中断ajax请求(长轮询)
                xmlHttpObjMap[xmlHttpObjSeq] = null; //清空
            }
        }
        xmlHttpObjSeq = 0;
        xmlHttpObjMap = {};
    };

    //重置sdk全局变量
    var clearSdk = function() {

        clearXmlHttpObjMap();

        //当前登录用户
        ctx = {
            sdkAppID: null,
            appIDAt3rd: null,
            identifier: null,
            identifierNick: null,
            accountType: null,
            userSig: null,
            contentType: 'json',
            apn: 1
        };
        opt = {};

        curSeq = 0;

        //ie8,9采用jsonp方法解决ajax跨域限制
        jsonpRequestId = 0; //jsonp请求id
        //最新jsonp请求返回的json数据
        jsonpLastRspData = null;

        apiReportItems = [];

        MsgManager.clear();

        //重置longpollingId
        LongPollingId = null;
    };

    //登录
    var _login = function(loginInfo, listeners, options, cbOk, cbErr) {
        clearSdk();

        if (options) opt = options;
        if (webim.Tool.getQueryString("isAccessFormalEnv") == 'false') {
            isAccessFormaEnvironment = false; //访问测试环境
            log.error("请切换为正式环境");
        }

        if (opt.isAccessFormalEnv == false) {
            log.error("请切换为正式环境");
            isAccessFormaEnvironment = opt.isAccessFormalEnv;
        }

        if (opt.isLogOn == false) {
            log.setOn(opt.isLogOn);
        }
        /*
         if(opt.emotions){
         emotions=opt.emotions;
         webim.Emotions= emotions;
         }
         if(opt.emotionDataIndexs){
         emotionDataIndexs=opt.emotionDataIndexs;
         webim.EmotionDataIndexs= emotionDataIndexs;
         }*/

        if (!loginInfo) {
            if (cbErr) {
                cbErr(tool.getReturnError("loginInfo is empty", -6));
                return;
            }
        }
        if (!loginInfo.sdkAppID) {
            if (cbErr) {
                cbErr(tool.getReturnError("loginInfo.sdkAppID is empty", -7));
                return;
            }
        }

        if (!loginInfo.accountType) {
            if (cbErr) {
                cbErr(tool.getReturnError("loginInfo.accountType is empty", -8));
                return;
            }
        }

        if (loginInfo.identifier) {
            ctx.identifier = loginInfo.identifier.toString();
        }
        if (loginInfo.identifier && !loginInfo.userSig) {
            if (cbErr) {
                cbErr(tool.getReturnError("loginInfo.userSig is empty", -9));
                return;
            }
        }
        if (loginInfo.userSig) {
            ctx.userSig = loginInfo.userSig.toString();
        }
        ctx.sdkAppID = loginInfo.sdkAppID;
        ctx.accountType = loginInfo.accountType;

        if (ctx.identifier && ctx.userSig) { //带登录态
            //登录
            proto_login(
                function(identifierNick, headurl) {
                    MsgManager.init(
                        listeners,
                        function(mmInitResp) {
                            if (cbOk) {
                                mmInitResp.identifierNick = identifierNick;
                                mmInitResp.headurl = headurl;
                                cbOk(mmInitResp);
                            }
                        }, cbErr
                    );
                },
                cbErr
            );
        } else { //不带登录态，进入直播场景sdk
            MsgManager.init(
                listeners,
                cbOk,
                cbErr
            );
        }
    };

    //初始化浏览器信息
    var initBrowserInfo = function() {
        //初始化浏览器类型
        BROWSER_INFO = tool.getBrowserInfo();
        log.info('BROWSER_INFO: type=' + BROWSER_INFO.type + ', ver=' + BROWSER_INFO.ver);
        if (BROWSER_INFO.type == "ie") {
            if (parseInt(BROWSER_INFO.ver) < 10) {
                lowerBR = true;
            }
        }
    };

    //接口质量上报
    var reportApiQuality = function(cmd, errorCode, errorInfo) {
        if (cmd == 'longpolling' && (errorCode == longPollingTimeOutErrorCode || errorCode == longPollingKickedErrorCode)) { //longpolling 返回60008错误可以视为正常,可以不上报
            return;
        }
        var eventId = CMD_EVENT_ID_MAP[cmd];
        if (eventId) {
            var reportTime = unixtime();
            var uniqKey = null;
            var msgCmdErrorCode = {
                'Code': errorCode,
                'ErrMsg': errorInfo
            };
            if (ctx.a2) {
                uniqKey = ctx.a2.substring(0, 10) + "_" + reportTime + "_" + createRandom();
            } else if (ctx.userSig) {
                uniqKey = ctx.userSig.substring(0, 10) + "_" + reportTime + "_" + createRandom();
            }

            if (uniqKey) {

                var rptEvtItem = {
                    "UniqKey": uniqKey,
                    "EventId": eventId,
                    "ReportTime": reportTime,
                    "MsgCmdErrorCode": msgCmdErrorCode
                };

                if (cmd == 'login') {
                    var loginApiReportItems = [];
                    loginApiReportItems.push(rptEvtItem);
                    var loginReportOpt = {
                        "EvtItems": loginApiReportItems,
                        "MainVersion": SDK.VERSION,
                        "Version": "0"
                    };
                    proto_reportApiQuality(loginReportOpt,
                        function(resp) {
                            loginApiReportItems = null; //
                        },
                        function(err) {
                            loginApiReportItems = null; //
                        }
                    );
                } else {
                    apiReportItems.push(rptEvtItem);
                    if (apiReportItems.length >= maxApiReportItemCount) { //累计一定条数再上报
                        var reportOpt = {
                            "EvtItems": apiReportItems,
                            "MainVersion": SDK.VERSION,
                            "Version": "0"
                        };
                        proto_reportApiQuality(reportOpt,
                            function(resp) {
                                apiReportItems = []; //清空
                            },
                            function(err) {
                                apiReportItems = []; //清空
                            }
                        );
                    }
                }

            }
        }
    };

    // REST API calls
    //上线
    var proto_login = function(cbOk, cbErr) {
        ConnManager.apiCall(SRV_NAME.OPEN_IM, "login", {
                "State": "Online"
            },
            function(loginResp) {
                if (loginResp.TinyId) {
                    ctx.tinyid = loginResp.TinyId;
                } else {
                    if (cbErr) {
                        cbErr(tool.getReturnError("TinyId is empty", -10));
                        return;
                    }
                }
                if (loginResp.A2Key) {
                    ctx.a2 = loginResp.A2Key;
                } else {
                    if (cbErr) {
                        cbErr(tool.getReturnError("A2Key is empty", -11));
                        return;
                    }
                }
                var tag_list = [
                    "Tag_Profile_IM_Nick",
                    "Tag_Profile_IM_Image"
                ];
                var options = {
                    'From_Account': ctx.identifier,
                    'To_Account': [ctx.identifier],
                    'LastStandardSequence': 0,
                    'TagList': tag_list
                };
                proto_getProfilePortrait(
                    options,
                    function(resp) {
                        var nick, gender, allowType;
                        if (resp.UserProfileItem && resp.UserProfileItem.length > 0) {
                            for (var i in resp.UserProfileItem) {
                                for (var j in resp.UserProfileItem[i].ProfileItem) {
                                    switch (resp.UserProfileItem[i].ProfileItem[j].Tag) {
                                        case 'Tag_Profile_IM_Nick':
                                            nick = resp.UserProfileItem[i].ProfileItem[j].Value;
                                            if (nick) ctx.identifierNick = nick;
                                            break;
                                        case 'Tag_Profile_IM_Image':
                                            image = resp.UserProfileItem[i].ProfileItem[j].Value;
                                            if (image) ctx.headurl = image;
                                            break;
                                    }
                                }
                            }
                        }
                        if (cbOk) cbOk(ctx.identifierNick, ctx.headurl); //回传当前用户昵称
                    }, cbErr);
            }, cbErr);
    };
    //下线
    var proto_logout = function(type, cbOk, cbErr) {
        if (!checkLogin(cbErr, false)) { //不带登录态
            clearSdk();
            if (cbOk) cbOk({
                'ActionStatus': ACTION_STATUS.OK,
                'ErrorCode': 0,
                'ErrorInfo': 'logout success'
            });
            return;
        }
        if (type == "all") {
            ConnManager.apiCall(SRV_NAME.OPEN_IM, "logout", {},
                function(resp) {
                    clearSdk();
                    if (cbOk) cbOk(resp);
                },
                cbErr);
        } else {
            ConnManager.apiCall(SRV_NAME.OPEN_IM, "longpollinglogout", {
                    LongPollingId: LongPollingId
                },
                function(resp) {
                    clearSdk();
                    if (cbOk) cbOk(resp);
                },
                cbErr);
        }
    };
    //发送消息，包括私聊和群聊
    var proto_sendMsg = function(msg, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        var msgInfo = null;

        switch (msg.sess.type()) {
            case SESSION_TYPE.C2C:
                msgInfo = {
                    'From_Account': ctx.identifier,
                    'To_Account': msg.sess.id().toString(),
                    'MsgTimeStamp': msg.time,
                    'MsgSeq': msg.seq,
                    'MsgRandom': msg.random,
                    'MsgBody': []
                };
                break;
            case SESSION_TYPE.GROUP:
                var subType = msg.getSubType();
                msgInfo = {
                    'GroupId': msg.sess.id().toString(),
                    'From_Account': ctx.identifier,
                    'Random': msg.random,
                    'MsgBody': []
                };
                switch (subType) {
                    case GROUP_MSG_SUB_TYPE.COMMON:
                        msgInfo.MsgPriority = "COMMON";
                        break;
                    case GROUP_MSG_SUB_TYPE.REDPACKET:
                        msgInfo.MsgPriority = "REDPACKET";
                        break;
                    case GROUP_MSG_SUB_TYPE.LOVEMSG:
                        msgInfo.MsgPriority = "LOVEMSG";
                        break;
                    case GROUP_MSG_SUB_TYPE.TIP:
                        log.error("不能主动发送群提示消息,subType=" + subType);
                        break;
                    default:
                        log.error("发送群消息时，出现未知子消息类型：subType=" + subType);
                        return;
                        break;
                }
                break;
            default:
                break;
        }

        for (var i in msg.elems) {
            var elem = msg.elems[i];
            var msgContent = null;
            var msgType = elem.type;
            switch (msgType) {
                case MSG_ELEMENT_TYPE.TEXT: //文本
                    msgContent = {
                        'Text': elem.content.text
                    };
                    break;
                case MSG_ELEMENT_TYPE.FACE: //表情
                    msgContent = {
                        'Index': elem.content.index,
                        'Data': elem.content.data
                    };
                    break;
                case MSG_ELEMENT_TYPE.IMAGE: //图片
                    var ImageInfoArray = [];
                    for (var j in elem.content.ImageInfoArray) {
                        ImageInfoArray.push({
                            'Type': elem.content.ImageInfoArray[j].type,
                            'Size': elem.content.ImageInfoArray[j].size,
                            'Width': elem.content.ImageInfoArray[j].width,
                            'Height': elem.content.ImageInfoArray[j].height,
                            'URL': elem.content.ImageInfoArray[j].url
                        });
                    }
                    msgContent = {
                        'ImageFormat': elem.content.ImageFormat,
                        'UUID': elem.content.UUID,
                        'ImageInfoArray': ImageInfoArray
                    };
                    break;
                case MSG_ELEMENT_TYPE.SOUND: //
                    log.warn('web端暂不支持发送语音消息');
                    continue;
                    break;
                case MSG_ELEMENT_TYPE.LOCATION: //
                    log.warn('web端暂不支持发送地理位置消息');
                    continue;
                    break;
                case MSG_ELEMENT_TYPE.FILE: //
                    msgContent = {
                        'UUID': elem.content.uuid,
                        'FileName': elem.content.name,
                        'FileSize': elem.content.size,
                        'DownloadFlag': elem.content.downFlag
                    };
                    break;
                case MSG_ELEMENT_TYPE.CUSTOM: //
                    msgContent = {
                        'Data': elem.content.data,
                        'Desc': elem.content.desc,
                        'Ext': elem.content.ext
                    };
                    msgType = MSG_ELEMENT_TYPE.CUSTOM;
                    break;
                default:
                    log.warn('web端暂不支持发送' + elem.type + '消息');
                    continue;
                    break;
            }

            if (msg.PushInfoBoolean) {
                msgInfo.OfflinePushInfo = msg.PushInfo; //当android终端进程被杀掉时才走push，IOS退到后台即可
            }

            msgInfo.MsgBody.push({
                'MsgType': msgType,
                'MsgContent': msgContent
            });
        }
        if (msg.sess.type() == SESSION_TYPE.C2C) { //私聊
            ConnManager.apiCall(SRV_NAME.OPEN_IM, "sendmsg", msgInfo, cbOk, cbErr);
        } else if (msg.sess.type() == SESSION_TYPE.GROUP) { //群聊
            ConnManager.apiCall(SRV_NAME.GROUP, "send_group_msg", msgInfo, cbOk, cbErr);
        }
    };
    //长轮询接口
    var proto_longPolling = function(options, cbOk, cbErr) {
        if (!isAccessFormaEnvironment && typeof stopPolling != "undefined" && stopPolling == true) {
            return;
        }
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.OPEN_IM, "longpolling", options, cbOk, cbErr, longPollingDefaultTimeOut, true);
    };

    //长轮询接口(拉取直播聊天室新消息)
    var proto_bigGroupLongPolling = function(options, cbOk, cbErr, timeout) {
        ConnManager.apiCall(SRV_NAME.BIG_GROUP_LONG_POLLING, "get_msg", options, cbOk, cbErr, timeout);
    };

    //拉取未读c2c消息接口
    var proto_getMsgs = function(cookie, syncFlag, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.OPEN_IM, "getmsg", {
                'Cookie': cookie,
                'SyncFlag': syncFlag
            },
            function(resp) {

                if (resp.MsgList && resp.MsgList.length) {
                    for (var i in resp.MsgList) {
                        tempC2CMsgList.push(resp.MsgList[i]);
                    }
                }
                if (resp.SyncFlag == 1) {
                    proto_getMsgs(resp.Cookie, resp.SyncFlag, cbOk, cbErr);
                } else {
                    resp.MsgList = tempC2CMsgList;
                    tempC2CMsgList = [];
                    if (cbOk) cbOk(resp);
                }
            },
            cbErr);
    };
    //C2C消息已读上报接口
    var proto_c2CMsgReaded = function(cookie, c2CMsgReadedItem, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        var tmpC2CMsgReadedItem = [];
        for (var i in c2CMsgReadedItem) {
            var item = {
                'To_Account': c2CMsgReadedItem[i].toAccount,
                'LastedMsgTime': c2CMsgReadedItem[i].lastedMsgTime
            };
            tmpC2CMsgReadedItem.push(item);
        }
        ConnManager.apiCall(SRV_NAME.OPEN_IM, "msgreaded", {
            C2CMsgReaded: {
                'Cookie': cookie,
                'C2CMsgReadedItem': tmpC2CMsgReadedItem
            }
        }, cbOk, cbErr);
    };

    //删除c2c消息
    var proto_deleteC2CMsg = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.OPEN_IM, "deletemsg", options,
            cbOk, cbErr);
    };

    //拉取c2c历史消息接口
    var proto_getC2CHistoryMsgs = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.OPEN_IM, "getroammsg", options,
            function(resp) {
                var reqMsgCount = options.MaxCnt;
                var complete = resp.Complete;
                var rspMsgCount = resp.MaxCnt;
                var msgKey = resp.MsgKey;
                var lastMsgTime = resp.LastMsgTime;

                if (resp.MsgList && resp.MsgList.length) {
                    for (var i in resp.MsgList) {
                        tempC2CHistoryMsgList.push(resp.MsgList[i]);
                    }
                }
                var netxOptions = null;
                if (complete == 0) { //还有历史消息可拉取
                    if (rspMsgCount < reqMsgCount) {
                        netxOptions = {
                            'Peer_Account': options.Peer_Account,
                            'MaxCnt': reqMsgCount - rspMsgCount,
                            'LastMsgTime': lastMsgTime,
                            'MsgKey': msgKey
                        };
                    }
                }

                if (netxOptions) { //继续拉取
                    proto_getC2CHistoryMsgs(netxOptions, cbOk, cbErr);
                } else {
                    resp.MsgList = tempC2CHistoryMsgList;
                    tempC2CHistoryMsgList = [];
                    if (cbOk) cbOk(resp);
                }
            },
            cbErr);
    };

    //群组接口
    //创建群组
    //协议参考：https://www.qcloud.com/doc/product/269/1615
    var proto_createGroup = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        var opt = {
            //必填    群组形态，包括Public（公开群），Private（私有群），ChatRoom（聊天室），AVChatRoom（互动直播聊天室）。
            'Type': options.Type,
            //必填    群名称，最长30字节。
            'Name': options.Name
        };
        var member_list = [];

        //Array 选填  初始群成员列表，最多500个。成员信息字段详情参见：群成员资料。
        for (var i = 0; i < options.MemberList.length; i++) {
            member_list.push({
                'Member_Account': options.MemberList[i]
            })
        }
        opt.MemberList = member_list;
        //选填    为了使得群组ID更加简单，便于记忆传播，腾讯云支持APP在通过REST API创建群组时自定义群组ID。详情参见：自定义群组ID。
        if (options.GroupId) {
            opt.GroupId = options.GroupId;
        }
        //选填    群主id，自动添加到群成员中。如果不填，群没有群主。
        if (options.Owner_Account) {
            opt.Owner_Account = options.Owner_Account;
        }
        //选填    群简介，最长240字节。
        if (options.Introduction) {
            opt.Introduction = options.Introduction;
        }
        //选填    群公告，最长300字节。
        if (options.Notification) {
            opt.Notification = options.Notification;
        }
        //选填    最大群成员数量，最大为10000，不填默认为2000个。
        if (options.MaxMemberCount) {
            opt.MaxMemberCount = options.MaxMemberCount;
        }
        //选填    申请加群处理方式。包含FreeAccess（自由加入），NeedPermission（需要验证），DisableApply（禁止加群），不填默认为NeedPermission（需要验证）。
        if (options.ApplyJoinOption) { //
            opt.ApplyJoinOption = options.ApplyJoinOption;
        }
        //Array 选填  群组维度的自定义字段，默认情况是没有的，需要开通，详情参见：自定义字段。
        if (options.AppDefinedData) {
            opt.AppDefinedData = options.AppDefinedData;
        }
        //选填    群头像URL，最长100字节。
        if (options.FaceUrl) {
            opt.FaceUrl = options.FaceUrl;
        }
        ConnManager.apiCall(SRV_NAME.GROUP, "create_group", opt,
            cbOk, cbErr);
    };

    //创建群组-高级接口
    //协议参考：https://www.qcloud.com/doc/product/269/1615
    var proto_createGroupHigh = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.GROUP, "create_group", options,
            cbOk, cbErr);
    };

    //修改群组基本资料
    //协议参考：https://www.qcloud.com/doc/product/269/1620
    var proto_modifyGroupBaseInfo = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "modify_group_base_info", options,
            cbOk, cbErr);
    };

    //申请加群
    var proto_applyJoinGroup = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "apply_join_group", {
                'GroupId': options.GroupId,
                'ApplyMsg': options.ApplyMsg,
                'UserDefinedField': options.UserDefinedField
            },
            cbOk, cbErr);
    };

    //申请加入大群
    var proto_applyJoinBigGroup = function(options, cbOk, cbErr) {
        var srvName;
        if (!checkLogin(cbErr, false)) { //未登录
            srvName = SRV_NAME.BIG_GROUP;
        } else { //已登录
            srvName = SRV_NAME.GROUP;
        }
        ConnManager.apiCall(srvName, "apply_join_group", {
                'GroupId': options.GroupId,
                'ApplyMsg': options.ApplyMsg,
                'UserDefinedField': options.UserDefinedField
            },
            function(resp) {
                if (resp.JoinedStatus && resp.JoinedStatus == 'JoinedSuccess') {
                    if (resp.LongPollingKey) {
                        MsgManager.setBigGroupLongPollingOn(true); //开启长轮询
                        MsgManager.setBigGroupLongPollingKey(resp.LongPollingKey); //更新大群长轮询key
                        MsgManager.setBigGroupLongPollingMsgMap(options.GroupId, 0); //收到的群消息置0
                        MsgManager.bigGroupLongPolling(); //开启长轮询
                    } else { //没有返回LongPollingKey，说明申请加的群不是直播聊天室(AVChatRoom)
                        cbErr && cbErr(tool.getReturnError("Join Group succeed; But the type of group is not AVChatRoom: groupid=" + options.GroupId, -12));
                        return;
                    }
                }
                if (cbOk) cbOk(resp);
            }, function(err) {

                if (cbErr) cbErr(err);
            });
    };

    //处理加群申请(同意或拒绝)
    var proto_handleApplyJoinGroupPendency = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "handle_apply_join_group", {
                'GroupId': options.GroupId,
                'Applicant_Account': options.Applicant_Account,
                'HandleMsg': options.HandleMsg,
                'Authentication': options.Authentication,
                'MsgKey': options.MsgKey,
                'ApprovalMsg': options.ApprovalMsg,
                'UserDefinedField': options.UserDefinedField
            },
            cbOk,
            function(err) {
                if (err.ErrorCode == 10024) { //apply has be handled
                    if (cbOk) {
                        var resp = {
                            'ActionStatus': ACTION_STATUS.OK,
                            'ErrorCode': 0,
                            'ErrorInfo': '该申请已经被处理过'
                        };
                        cbOk(resp);
                    }
                } else {
                    if (cbErr) cbErr(err);
                }
            }
        );
    };

    //获取群组未决列表
    var proto_getPendencyGroup = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "get_pendency", {
                'StartTime': options.StartTime,
                'Limit': options.Limit,
                'Handle_Account': ctx.identifier
            },
            cbOk,
            function(err) {

            }
        );
    };


    //群组未决已经上报
    var proto_getPendencyGroupRead = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "report_pendency", {
                'ReportTime': options.ReportTime,
                'From_Account': ctx.identifier
            },
            cbOk,
            function(err) {

            }
        );
    };

    //处理被邀请进群申请(同意或拒绝)
    var proto_handleInviteJoinGroupRequest = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "handle_invite_join_group", {
                'GroupId': options.GroupId,
                'Inviter_Account': options.Inviter_Account,
                'HandleMsg': options.HandleMsg,
                'Authentication': options.Authentication,
                'MsgKey': options.MsgKey,
                'ApprovalMsg': options.ApprovalMsg,
                'UserDefinedField': options.UserDefinedField
            },
            cbOk,
            function(err) {

            }
        );
    };

    //主动退群
    var proto_quitGroup = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "quit_group", {
                'GroupId': options.GroupId
            },
            cbOk, cbErr);
    };

    //退出大群
    var proto_quitBigGroup = function(options, cbOk, cbErr) {
        var srvName;
        if (!checkLogin(cbErr, false)) { //未登录
            srvName = SRV_NAME.BIG_GROUP;
        } else { //已登录
            srvName = SRV_NAME.GROUP;
        }
        ConnManager.apiCall(srvName, "quit_group", {
                'GroupId': options.GroupId
            },
            function(resp) {
                //重置当前再请求中的ajax
                //clearXmlHttpObjMap();
                //退出大群成功之后需要重置长轮询信息
                MsgManager.resetBigGroupLongPollingInfo();
                if (cbOk) cbOk(resp);
            },
            cbErr);
    };
    //查找群(按名称)
    var proto_searchGroupByName = function(options, cbOk, cbErr) {
        ConnManager.apiCall(SRV_NAME.GROUP, "search_group", options, cbOk, cbErr);
    };

    //获取群组公开资料
    var proto_getGroupPublicInfo = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "get_group_public_info", {
                'GroupIdList': options.GroupIdList,
                'ResponseFilter': {
                    'GroupBasePublicInfoFilter': options.GroupBasePublicInfoFilter
                }
            },
            function(resp) {
                resp.ErrorInfo = '';
                if (resp.GroupInfo) {
                    for (var i in resp.GroupInfo) {
                        var errorCode = resp.GroupInfo[i].ErrorCode;
                        if (errorCode > 0) {
                            resp.ActionStatus = ACTION_STATUS.FAIL;
                            resp.GroupInfo[i].ErrorInfo = "[" + errorCode + "]" + resp.GroupInfo[i].ErrorInfo;
                            resp.ErrorInfo += resp.GroupInfo[i].ErrorInfo + '\n';
                        }
                    }
                }
                if (resp.ActionStatus == ACTION_STATUS.FAIL) {
                    if (cbErr) {
                        cbErr(resp);
                    }
                } else if (cbOk) {
                    cbOk(resp);
                }

            },
            cbErr);
    };

    //获取群组详细资料--高级
    //请求协议参考：https://www.qcloud.com/doc/product/269/1616
    var proto_getGroupInfo = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        var opt = {
            'GroupIdList': options.GroupIdList,
            'ResponseFilter': {
                'GroupBaseInfoFilter': options.GroupBaseInfoFilter,
                'MemberInfoFilter': options.MemberInfoFilter
            }
        };
        if (options.AppDefinedDataFilter_Group) {
            opt.ResponseFilter.AppDefinedDataFilter_Group = options.AppDefinedDataFilter_Group;
        }
        if (options.AppDefinedDataFilter_GroupMember) {
            opt.ResponseFilter.AppDefinedDataFilter_GroupMember = options.AppDefinedDataFilter_GroupMember;
        }
        ConnManager.apiCall(SRV_NAME.GROUP, "get_group_info", opt,
            cbOk, cbErr);
    };

    //获取群组成员-高级接口
    //协议参考：https://www.qcloud.com/doc/product/269/1617
    var proto_getGroupMemberInfo = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "get_group_member_info", {
                'GroupId': options.GroupId,
                'Offset': options.Offset,
                'Limit': options.Limit,
                'MemberInfoFilter': options.MemberInfoFilter,
                'MemberRoleFilter': options.MemberRoleFilter,
                'AppDefinedDataFilter_GroupMember': options.AppDefinedDataFilter_GroupMember
            },
            cbOk, cbErr);
    };


    //增加群组成员
    //协议参考：https://www.qcloud.com/doc/product/269/1621
    var proto_addGroupMember = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "add_group_member", {
                'GroupId': options.GroupId,
                'Silence': options.Silence,
                'MemberList': options.MemberList
            },
            cbOk, cbErr);
    };
    //修改群组成员资料
    //协议参考：https://www.qcloud.com/doc/product/269/1623
    var proto_modifyGroupMember = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        var opt = {};
        if (options.GroupId) {
            opt.GroupId = options.GroupId;
        }
        if (options.Member_Account) {
            opt.Member_Account = options.Member_Account;
        }
        //Admin或者Member
        if (options.Role) {
            opt.Role = options.Role;
        }
        // AcceptAndNotify代表解收并提示消息，Discard代表不接收也不提示消息，AcceptNotNotify代表接收消息但不提示
        if (options.MsgFlag) {
            opt.MsgFlag = options.MsgFlag;
        }
        if (options.ShutUpTime) { //禁言时间
            opt.ShutUpTime = options.ShutUpTime;
        }
        if (options.NameCard) { //群名片,最大不超过50个字节
            opt.NameCard = options.NameCard;
        }
        if (options.AppMemberDefinedData) { //群成员维度的自定义字段，默认情况是没有的，需要开通
            opt.AppMemberDefinedData = options.AppMemberDefinedData;
        }
        ConnManager.apiCall(SRV_NAME.GROUP, "modify_group_member_info", opt,
            cbOk, cbErr);
    };
    //删除群组成员
    //协议参考：https://www.qcloud.com/doc/product/269/1622
    var proto_deleteGroupMember = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "delete_group_member", {
                'GroupId': options.GroupId,
                'Silence': options.Silence,
                'MemberToDel_Account': options.MemberToDel_Account,
                'Reason': options.Reason
            },
            cbOk, cbErr);
    };
    //解散群组
    //协议参考：https://www.qcloud.com/doc/product/269/1624
    var proto_destroyGroup = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "destroy_group", {
                'GroupId': options.GroupId
            },
            cbOk, cbErr);
    };
    //转让群组
    //协议参考：https://www.qcloud.com/doc/product/269/1633
    var proto_changeGroupOwner = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.GROUP, "change_group_owner", options, cbOk, cbErr);
    };
    //获取用户所加入的群组-高级接口
    //协议参考：https://www.qcloud.com/doc/product/269/1625
    var proto_getJoinedGroupListHigh = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "get_joined_group_list", {
                'Member_Account': options.Member_Account,
                'Limit': options.Limit,
                'Offset': options.Offset,
                'GroupType': options.GroupType,
                'ResponseFilter': {
                    'GroupBaseInfoFilter': options.GroupBaseInfoFilter,
                    'SelfInfoFilter': options.SelfInfoFilter
                }
            },
            cbOk, cbErr);
    };
    //查询一组UserId在群中的身份
    //协议参考：https://www.qcloud.com/doc/product/269/1626
    var proto_getRoleInGroup = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "get_role_in_group", {
                'GroupId': options.GroupId,
                'User_Account': options.User_Account
            },
            cbOk, cbErr);
    };
    //设置取消成员禁言时间
    //协议参考：https://www.qcloud.com/doc/product/269/1627
    var proto_forbidSendMsg = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;

        ConnManager.apiCall(SRV_NAME.GROUP, "forbid_send_msg", {
                'GroupId': options.GroupId,
                'Members_Account': options.Members_Account,
                'ShutUpTime': options.ShutUpTime //单位为秒，为0时表示取消禁言
            },
            cbOk, cbErr);
    };

    //发送自定义群系统通知
    var proto_sendCustomGroupNotify = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.GROUP, "send_group_system_notification", options,
            cbOk, cbErr);
    };

    //拉取群消息接口
    var proto_getGroupMsgs = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.GROUP, "group_msg_get", {
                "GroupId": options.GroupId,
                "ReqMsgSeq": options.ReqMsgSeq,
                "ReqMsgNumber": options.ReqMsgNumber
            },
            cbOk, cbErr);
    };
    //群消息已读上报接口
    var proto_groupMsgReaded = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.GROUP, "msg_read_report", {
                'GroupId': options.GroupId,
                'MsgReadedSeq': options.MsgReadedSeq
            },
            cbOk, cbErr);
    };
    //end

    //好友接口
    //处理好友接口返回的错误码
    var convertErrorEn2ZhFriend = function(resp) {
        var errorAccount = [];
        if (resp.Fail_Account && resp.Fail_Account.length) {
            errorAccount = resp.Fail_Account;
        }
        if (resp.Invalid_Account && resp.Invalid_Account.length) {
            for (var k in resp.Invalid_Account) {
                errorAccount.push(resp.Invalid_Account[k]);
            }
        }
        if (errorAccount.length) {
            resp.ActionStatus = ACTION_STATUS.FAIL;
            resp.ErrorCode = ERROR_CODE_CUSTOM;
            resp.ErrorInfo = '';
            for (var i in errorAccount) {
                var failCount = errorAccount[i];
                for (var j in resp.ResultItem) {
                    if (resp.ResultItem[j].To_Account == failCount) {

                        var resultCode = resp.ResultItem[j].ResultCode;
                        resp.ResultItem[j].ResultInfo = "[" + resultCode + "]" + resp.ResultItem[j].ResultInfo;
                        resp.ErrorInfo += resp.ResultItem[j].ResultInfo + "\n";
                        break;
                    }
                }
            }
        }
        return resp;
    };
    //添加好友
    var proto_applyAddFriend = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.FRIEND, "friend_add", {
                'From_Account': ctx.identifier,
                'AddFriendItem': options.AddFriendItem
            },
            function(resp) {
                var newResp = convertErrorEn2ZhFriend(resp);
                if (newResp.ActionStatus == ACTION_STATUS.FAIL) {
                    if (cbErr) cbErr(newResp);
                } else if (cbOk) {
                    cbOk(newResp);
                }
            }, cbErr);
    };
    //删除好友
    var proto_deleteFriend = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.FRIEND, "friend_delete", {
                'From_Account': ctx.identifier,
                'To_Account': options.To_Account,
                'DeleteType': options.DeleteType
            },
            function(resp) {
                var newResp = convertErrorEn2ZhFriend(resp);
                if (newResp.ActionStatus == ACTION_STATUS.FAIL) {
                    if (cbErr) cbErr(newResp);
                } else if (cbOk) {
                    cbOk(newResp);
                }
            }, cbErr);
    };

    //删除会话
    var proto_deleteChat = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        console.log("options=="+JSON.stringify(options));
		console.log("options.chatType=="+options.chatType);
        if (options.chatType == 1) {
            ConnManager.apiCall(SRV_NAME.DEL_CHAT, "delete", { 
                    'From_Account': ctx.identifier,
                    'Type': options.chatType,
                    'To_Account': options.To_Account
                },
                cbOk, cbErr);
        } else {
            ConnManager.apiCall(SRV_NAME.DEL_CHAT, "delete", {
                    'From_Account': ctx.identifier,
                    'Type': options.chatType,
                    'ToGroupid': options.To_Account
                },
                cbOk, cbErr);

        }

    };

    //获取好友申请
    var proto_getPendency = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.FRIEND, "pendency_get", {
                "From_Account": ctx.identifier,
                "PendencyType": options.PendencyType,
                "StartTime": options.StartTime,
                "MaxLimited": options.MaxLimited,
                "LastSequence": options.LastSequence
            },
            cbOk, cbErr);
    };
    //好友申请已读上报
    var proto_getPendencyReport = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.FRIEND, "PendencyReport", {
                "From_Account": ctx.identifier,
                "LatestPendencyTimeStamp": options.LatestPendencyTimeStamp
            },
            cbOk, cbErr);
    };
    //删除好友申请
    var proto_deletePendency = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.FRIEND, "pendency_delete", {
                "From_Account": ctx.identifier,
                "PendencyType": options.PendencyType,
                "To_Account": options.To_Account

            },
            function(resp) {
                var newResp = convertErrorEn2ZhFriend(resp);
                if (newResp.ActionStatus == ACTION_STATUS.FAIL) {
                    if (cbErr) cbErr(newResp);
                } else if (cbOk) {
                    cbOk(newResp);
                }
            }, cbErr);
    };
    //处理好友申请
    var proto_responseFriend = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.FRIEND, "friend_response", {
                'From_Account': ctx.identifier,
                'ResponseFriendItem': options.ResponseFriendItem
            },
            function(resp) {
                var newResp = convertErrorEn2ZhFriend(resp);
                if (newResp.ActionStatus == ACTION_STATUS.FAIL) {
                    if (cbErr) cbErr(newResp);
                } else if (cbOk) {
                    cbOk(newResp);
                }
            }, cbErr);
    };
    //我的好友
    var proto_getAllFriend = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.FRIEND, "friend_get_all", {
                'From_Account': ctx.identifier,
                'TimeStamp': options.TimeStamp,
                'StartIndex': options.StartIndex,
                'GetCount': options.GetCount,
                'LastStandardSequence': options.LastStandardSequence,
                'TagList': options.TagList
            },
            cbOk, cbErr);
    };

    //资料接口
    //查看个人资料
    var proto_getProfilePortrait = function(options, cbOk, cbErr) {
        if (options.To_Account.length > 100) {
            options.To_Account.length = 100;
            log.error('获取用户资料人数不能超过100人')
        }
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.PROFILE, "portrait_get", {
                'From_Account': ctx.identifier,
                'To_Account': options.To_Account,
                //'LastStandardSequence':options.LastStandardSequence,
                'TagList': options.TagList
            },
            function(resp) {
                var errorAccount = [];
                if (resp.Fail_Account && resp.Fail_Account.length) {
                    errorAccount = resp.Fail_Account;
                }
                if (resp.Invalid_Account && resp.Invalid_Account.length) {
                    for (var k in resp.Invalid_Account) {
                        errorAccount.push(resp.Invalid_Account[k]);
                    }
                }
                if (errorAccount.length) {
                    resp.ActionStatus = ACTION_STATUS.FAIL;
                    resp.ErrorCode = ERROR_CODE_CUSTOM;
                    resp.ErrorInfo = '';
                    for (var i in errorAccount) {
                        var failCount = errorAccount[i];
                        for (var j in resp.UserProfileItem) {
                            if (resp.UserProfileItem[j].To_Account == failCount) {
                                var resultCode = resp.UserProfileItem[j].ResultCode;
                                resp.UserProfileItem[j].ResultInfo = "[" + resultCode + "]" + resp.UserProfileItem[j].ResultInfo;
                                resp.ErrorInfo += "账号:" + failCount + "," + resp.UserProfileItem[j].ResultInfo + "\n";
                                break;
                            }
                        }
                    }
                }
                if (resp.ActionStatus == ACTION_STATUS.FAIL) {
                    if (cbErr) cbErr(resp);
                } else if (cbOk) {
                    cbOk(resp);
                }
            },
            cbErr);
    };

    //设置个人资料
    var proto_setProfilePortrait = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.PROFILE, "portrait_set", {
                'From_Account': ctx.identifier,
                'ProfileItem': options.ProfileItem
            },
            function(resp) {
                for (var i in options.ProfileItem) {
                    var profile = options.ProfileItem[i];
                    if (profile.Tag == 'Tag_Profile_IM_Nick') {
                        ctx.identifierNick = profile.Value; //更新昵称
                        break;
                    }
                }
                if (cbOk) cbOk(resp);
            }, cbErr);
    };

    //增加黑名单
    var proto_addBlackList = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.FRIEND, "black_list_add", {
                'From_Account': ctx.identifier,
                'To_Account': options.To_Account
            },
            function(resp) {
                var newResp = convertErrorEn2ZhFriend(resp);
                if (newResp.ActionStatus == ACTION_STATUS.FAIL) {
                    if (cbErr) cbErr(newResp);
                } else if (cbOk) {
                    cbOk(newResp);
                }
            }, cbErr);
    };

    //删除黑名单
    var proto_deleteBlackList = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.FRIEND, "black_list_delete", {
                'From_Account': ctx.identifier,
                'To_Account': options.To_Account
            },
            function(resp) {
                var newResp = convertErrorEn2ZhFriend(resp);
                if (newResp.ActionStatus == ACTION_STATUS.FAIL) {
                    if (cbErr) cbErr(newResp);
                } else if (cbOk) {
                    cbOk(newResp);
                }
            }, cbErr);
    };

    //我的黑名单
    var proto_getBlackList = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.FRIEND, "black_list_get", {
                'From_Account': ctx.identifier,
                'StartIndex': options.StartIndex,
                'MaxLimited': options.MaxLimited,
                'LastSequence': options.LastSequence
            },
            cbOk, cbErr);
    };

    //获取最近联系人
    var proto_getRecentContactList = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.RECENT_CONTACT, "get", {
                'From_Account': ctx.identifier,
                'Count': options.Count
            },
            cbOk, cbErr);
    };

    //上传图片或文件
    var proto_uploadPic = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        var cmdName;
        if (isAccessFormalEnv()) {
            cmdName = 'pic_up';
        } else {
            cmdName = 'pic_up_test';
        }
        ConnManager.apiCall(SRV_NAME.PIC, cmdName, {
                'App_Version': VERSION_INFO.APP_VERSION,
                'From_Account': ctx.identifier,
                'To_Account': options.To_Account,
                'Seq': options.Seq,
                'Timestamp': options.Timestamp,
                'Random': options.Random,
                'File_Str_Md5': options.File_Str_Md5,
                'File_Size': options.File_Size,
                'File_Type': options.File_Type,
                'Server_Ver': VERSION_INFO.SERVER_VERSION,
                'Auth_Key': authkey,
                'Busi_Id': options.Busi_Id,
                'PkgFlag': options.PkgFlag,
                'Slice_Offset': options.Slice_Offset,
                'Slice_Size': options.Slice_Size,
                'Slice_Data': options.Slice_Data
            },
            cbOk, cbErr);
    };

    //获取语音和文件下载IP和authkey
    var proto_getIpAndAuthkey = function(cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.OPEN_IM, "authkey", {}, cbOk, cbErr);
    };

    //接口质量上报
    var proto_reportApiQuality = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.IM_OPEN_STAT, "web_report", options, cbOk, cbErr);
    };


    var proto_getLongPollingId = function(options, cbOk, cbErr) {
        if (!checkLogin(cbErr, true)) return;
        ConnManager.apiCall(SRV_NAME.OPEN_IM, "getlongpollingid", {},
            function(resp) {
                cbOk && cbOk(resp);
            }, cbErr);
    }


    var proto_applyDownload = function(options, cbOk, cbErr) {
        //把下载地址push到map中
        ConnManager.apiCall(SRV_NAME.PIC, "apply_download", options, cbOk, cbErr);
    }

    //end
    initBrowserInfo();
    // singleton object ConnManager
    var ConnManager = lowerBR == false ? new function() {
            var onConnCallback = null; //回调函数
            this.init = function(onConnNotify, cbOk, cbErr) {
                if (onConnNotify) onConnCallback = onConnNotify;
            };
            this.callBack = function(info) {
                if (onConnCallback) onConnCallback(info);
            };
            this.clear = function() {
                onConnCallback = null;
            };
            //请求后台服务接口
            this.apiCall = function(type, cmd, data, cbOk, cbErr, timeout, isLongPolling) {
                //封装后台服务接口地址
                var url = getApiUrl(type, cmd, cbOk, cbErr);
                if (url == false) return;
                //发起ajax请求
//              console.log("webim---url===>"+url);
//                console.log("webim---data===>"+JSON.stringify(data));
                ajaxRequestJson("POST", url, data, timeout, isLongPolling, function(resp) {
                    var errorCode = null,
                        tempErrorInfo = '';
                    if (cmd == 'pic_up') {
                        data.Slice_Data = '';
                    }
                    var info = "\n request url: \n" + url + "\n request body: \n" + JSON.stringify(data) + "\n response: \n" + JSON.stringify(resp);
                    //成功
                    if (resp.ActionStatus == ACTION_STATUS.OK) {
                        log.info("[" + type + "][" + cmd + "]success: " + info);
                        if (cbOk) cbOk(resp); //回调
                        errorCode = 0;
                        tempErrorInfo = '';
                    } else {
                        errorCode = resp.ErrorCode;
                        tempErrorInfo = resp.ErrorInfo;
                        //报错
                        if (cbErr) {
                            resp.SrcErrorInfo = resp.ErrorInfo;
                            resp.ErrorInfo = "[" + type + "][" + cmd + "]failed: " + info;
                            if (cmd != 'longpolling' || resp.ErrorCode != longPollingTimeOutErrorCode) {
                                log.error(resp.ErrorInfo);
                            }
                            cbErr(resp);
                        }
                    }
                    reportApiQuality(cmd, errorCode, tempErrorInfo); //接口质量上报
                }, function(err) {
                    cbErr && cbErr(err);
                    reportApiQuality(cmd, err.ErrorCode, err.ErrorInfo); //接口质量上报
                });
            };
        } : new function() {
            var onConnCallback = null; //回调函数
            this.init = function(onConnNotify, cbOk, cbErr) {
                if (onConnNotify) onConnCallback = onConnNotify;
            };
            this.callBack = function(info) {
                if (onConnCallback) onConnCallback(info);
            };
            this.clear = function() {
                onConnCallback = null;
            };
            //请求后台服务接口
            this.apiCall = function(type, cmd, data, cbOk, cbErr, timeout, isLongPolling) {
                //封装后台服务接口地址
                var url = getApiUrl(type, cmd, cbOk, cbErr);
                if (url == false) return;
                //发起jsonp请求
                var reqId = jsonpRequestId++,
                    cbkey = 'jsonpcallback', // the 'callback' key
                    cbval = 'jsonpRequest' + reqId, // the 'callback' value
                    script = document.createElement('script'),
                    loaded = 0;

                window[cbval] = jsonpCallback;
                script.type = 'text/javascript';
                url = url + "&" + cbkey + "=" + cbval + "&jsonpbody=" + encodeURIComponent(JSON.stringify(data));
                script.src = url;
                script.async = true;

                if (typeof script.onreadystatechange !== 'undefined') {
                    // need this for IE due to out-of-order onreadystatechange(), binding script
                    // execution to an event listener gives us control over when the script
                    // is executed. See http://jaubourg.net/2010/07/loading-script-as-onclick-handler-of.html
                    script.event = 'onclick';
                    script.htmlFor = script.id = '_jsonpRequest_' + reqId;
                }

                script.onload = script.onreadystatechange = function() {
                    if ((this.readyState && this.readyState !== 'complete' && this.readyState !== 'loaded') || loaded) {
                        return false;
                    }
                    script.onload = script.onreadystatechange = null;
                    script.onclick && script.onclick();
                    // Call the user callback with the last value stored and clean up values and scripts.
                    var resp = jsonpLastRspData;
                    var info = "\n request url: \n" + url + "\n request body: \n" + JSON.stringify(data) + "\n response: \n" + JSON.stringify(resp);
                    if (resp.ActionStatus == ACTION_STATUS.OK) {
                        log.info("[" + type + "][" + cmd + "]success: " + info);
                        cbOk && cbOk(resp);
                    } else {
                        resp.ErrorInfo = "[" + type + "][" + cmd + "]failed " + info;
                        if (cmd != 'longpolling' || resp.ErrorCode != longPollingTimeOutErrorCode) {
                            log.error(resp.ErrorInfo);
                        } else {
                            log.warn("[" + type + "][" + cmd + "]success: " + info);
                        }
                        cbErr && cbErr(resp);
                    }
                    jsonpLastRspData = undefined;
                    document.body.removeChild(script);
                    loaded = 1;
                };

                // Add the script to the DOM head
                document.body.appendChild(script);
            };
        };
    // class Session
    var Session = function(type, id, name, icon, time, seq) {
        this._impl = {
            skey: Session.skey(type, id),
            type: type,
            id: id,
            name: name,
            icon: icon,
            unread: 0, //未读消息数
            isAutoRead: false,
            time: time >= 0 ? time : 0,
            curMaxMsgSeq: seq >= 0 ? seq : 0,
            msgs: [],
            isFinished: 1
        };
    };
    Session.skey = function(type, id) {
        return type + id;
    };
    Session.prototype.type = function() {
        return this._impl.type;
    };
    Session.prototype.id = function() {
        return this._impl.id;
    };
    Session.prototype.name = function() {
        return this._impl.name;
    };
    Session.prototype.icon = function() {
        return this._impl.icon;
    };
    Session.prototype.unread = function(val) {
        if (typeof val !== 'undefined') {
            this._impl.unread = val;
        } else {
            return this._impl.unread;
        }
    };
    Session.prototype.isFinished = function(val) {
        if (typeof val !== 'undefined') {
            this._impl.isFinished = val;
        } else {
            return this._impl.isFinished;
        }
    };
    Session.prototype.time = function() {
        return this._impl.time;
    };
    Session.prototype.curMaxMsgSeq = function(seq) {
        if (typeof seq !== 'undefined') {
            this._impl.curMaxMsgSeq = seq;
        } else {
            return this._impl.curMaxMsgSeq;
        }
    };
    Session.prototype.msgCount = function() {
        return this._impl.msgs.length;
    };
    Session.prototype.msg = function(index) {
        return this._impl.msgs[index];
    };
    Session.prototype.msgs = function() {
        return this._impl.msgs;
    };
    Session.prototype._impl_addMsg = function(msg) {
        this._impl.msgs.push(msg);
        //if (!msg.isSend && msg.time > this._impl.time)
        if (msg.time > this._impl.time)
            this._impl.time = msg.time;
        //if (!msg.isSend && msg.seq > this._impl.curMaxMsgSeq)
        if (msg.seq > this._impl.curMaxMsgSeq)
            this._impl.curMaxMsgSeq = msg.seq;
        //自己发送的消息不计入未读数
        if (!msg.isSend && !this._impl.isAutoRead) {
            this._impl.unread++;
        }
    };
    //class C2CMsgReadedItem
    var C2CMsgReadedItem = function(toAccount, lastedMsgTime) {
        this.toAccount = toAccount;
        this.lastedMsgTime = lastedMsgTime;
    }

    var calcUniqId = function(num1, num2) {
        var str1 = parseInt(num1).toString(2) + '00000000000000000000000000000000';
        var str2 = parseInt(num2).toString(2);
        var arr1 = str1.split('').reverse();
        var arr2 = str2.split('').reverse();
        var res = [];
        var length = arr1.length > arr2.length ? arr1.length : arr2.length;
        for (var a = 0; a < length; a++) {
            sig = Number(arr1[a] || 0) || Number(arr2[a] || 0);
            res.push(sig);
        }
        var numstr = res.reverse().join("");
        var long = {
            high: "0x" + parseInt(numstr.substr(0, numstr.length - 32), 2).toString(16),
            low: "0x" + parseInt(numstr.substr(numstr.length - 32 - 1), 2).toString(16)
        }
        var longVal = new Long(long.low, long.high, true);
        return longVal.toString();
    };
    // class Msg
    var Msg = function(sess, isSend, seq, random, time, fromAccount, subType, fromAccountNick) {
        this.sess = sess;
        this.subType = subType >= 0 ? subType : 0; //消息类型,c2c消息时，type取值为0；group消息时，type取值0和1，0-普通群消息，1-群提示消息
        this.fromAccount = fromAccount;
        this.fromAccountNick = fromAccountNick ? fromAccountNick : fromAccount;
        this.isSend = Boolean(isSend);
        this.seq = seq >= 0 ? seq : nextSeq();
        this.random = random >= 0 ? random : createRandom();
        this.time = time >= 0 ? time : unixtime();
        this.elems = [];
        var type = sess.type();
        switch (type) {
            case SESSION_TYPE.GROUP:
                this.uniqueId = calcUniqId(this.seq, this.random);
                break;
            case SESSION_TYPE.C2C:
            default:
                this.uniqueId = calcUniqId(this.time, this.random);
                break;
        }


    };
    Msg.prototype.getSession = function() {
        return this.sess;
    };
    Msg.prototype.getType = function() {
        return this.subType;
    };
    Msg.prototype.getSubType = function() {
        return this.subType;
    };
    Msg.prototype.getFromAccount = function() {
        return this.fromAccount;
    };
    Msg.prototype.getFromAccountNick = function() {
        return this.fromAccountNick;
    };
    Msg.prototype.getIsSend = function() {
        return this.isSend;
    };
    Msg.prototype.getSeq = function() {
        return this.seq;
    };
    Msg.prototype.getTime = function() {
        return this.time;
    };
    Msg.prototype.getRandom = function() {
        return this.random;
    };
    Msg.prototype.getElems = function() {
        return this.elems;
    };
    Msg.prototype.getMsgUniqueId = function() {
        return this.uniqueId;
    };
    //文本
    Msg.prototype.addText = function(text) {
        this.addElem(new webim.Msg.Elem(MSG_ELEMENT_TYPE.TEXT, text));
    };
    //表情
    Msg.prototype.addFace = function(face) {
        this.addElem(new webim.Msg.Elem(MSG_ELEMENT_TYPE.FACE, face));
    };
    //图片
    Msg.prototype.addImage = function(image) {
        this.addElem(new webim.Msg.Elem(MSG_ELEMENT_TYPE.IMAGE, image));
    };
    //地理位置
    Msg.prototype.addLocation = function(location) {
        this.addElem(new webim.Msg.Elem(MSG_ELEMENT_TYPE.LOCATION, location));
    };
    //文件
    Msg.prototype.addFile = function(file) {
        this.addElem(new webim.Msg.Elem(MSG_ELEMENT_TYPE.FILE, file));
    };
    //自定义
    Msg.prototype.addCustom = function(custom) {
        this.addElem(new webim.Msg.Elem(MSG_ELEMENT_TYPE.CUSTOM, custom));
    };
    Msg.prototype.addElem = function(elem) {
        this.elems.push(elem);
    };
    Msg.prototype.toHtml = function() {
        var html = "";
        for (var i in this.elems) {
            var elem = this.elems[i];
            html += elem.toHtml();
        }
        return html;
    };

    // class Msg.Elem
    Msg.Elem = function(type, value) {
        this.type = type;
        this.content = value;
    };
    Msg.Elem.prototype.getType = function() {
        return this.type;
    };
    Msg.Elem.prototype.getContent = function() {
        return this.content;
    };
    Msg.Elem.prototype.toHtml = function() {
        var html;
        html = this.content.toHtml();
        return html;
    };

    // class Msg.Elem.Text
    Msg.Elem.Text = function(text) {
        this.text = tool.xssFilter(text);
    };
    Msg.Elem.Text.prototype.getText = function() {
        return this.text;
    };
    Msg.Elem.Text.prototype.toHtml = function() {
        return this.text;
    };

    // class Msg.Elem.Face
    Msg.Elem.Face = function(index, data) {
        this.index = index;
        this.data = data;
    };
    Msg.Elem.Face.prototype.getIndex = function() {
        return this.index;
    };
    Msg.Elem.Face.prototype.getData = function() {
        return this.data;
    };
    Msg.Elem.Face.prototype.toHtml = function() {
        var faceUrl = null;
        var index = emotionDataIndexs[this.data];
        var emotion = emotions[index];
        if (emotion && emotion[1]) {
            faceUrl = emotion[1];
        }
        if (faceUrl) {
            return "<img style='width:30px;height:30px' src='" + faceUrl + "'/>";
        } else {
            return this.data;
        }
    };
    // 地理位置消息 class Msg.Elem.Location
    Msg.Elem.Location = function(longitude, latitude, desc) {
        this.latitude = latitude; //纬度
        this.longitude = longitude; //经度
        this.desc = desc; //描述
    };
    Msg.Elem.Location.prototype.getLatitude = function() {
        return this.latitude;
    };
    Msg.Elem.Location.prototype.getLongitude = function() {
        return this.longitude;
    };
    Msg.Elem.Location.prototype.getDesc = function() {
        return this.desc;
    };
    Msg.Elem.Location.prototype.toHtml = function() {
        return '经度=' + this.longitude + ',纬度=' + this.latitude + ',描述=' + this.desc;
    };

    //图片消息
    // class Msg.Elem.Images
    Msg.Elem.Images = function(imageId, format) {
        this.UUID = imageId;
        if (typeof format !== 'number') {
            format = parseInt(IMAGE_FORMAT[format] || IMAGE_FORMAT['UNKNOWN'], 10);
        }
        this.ImageFormat = format;
        this.ImageInfoArray = [];
    };
    Msg.Elem.Images.prototype.addImage = function(image) {
        this.ImageInfoArray.push(image);
    };
    Msg.Elem.Images.prototype.toHtml = function() {
        var smallImage = this.getImage(IMAGE_TYPE.SMALL);
        var bigImage = this.getImage(IMAGE_TYPE.LARGE);
        var oriImage = this.getImage(IMAGE_TYPE.ORIGIN);
        if (!bigImage) {
            bigImage = smallImage;
        }
        if (!oriImage) {
            oriImage = smallImage;
        }
        return "<img src='" + smallImage.getUrl() + "#" + bigImage.getUrl() + "#" + oriImage.getUrl() + "' style='CURSOR: hand' id='" + this.getImageId() + "' bigImgUrl='" + bigImage.getUrl() + "' onclick='imageClick(this)' />";

    };
    Msg.Elem.Images.prototype.getImageId = function() {
        return this.UUID;
    };
    Msg.Elem.Images.prototype.getImageFormat = function() {
        return this.ImageFormat;
    };
    Msg.Elem.Images.prototype.getImage = function(type) {
        for (var i in this.ImageInfoArray) {
            if (this.ImageInfoArray[i].getType() == type) {
                return this.ImageInfoArray[i];
            }
        }
        return null;
    };
    // class Msg.Elem.Images.Image
    Msg.Elem.Images.Image = function(type, size, width, height, url) {
        this.type = type;
        this.size = size;
        this.width = width;
        this.height = height;
        this.url = url;
    };
    Msg.Elem.Images.Image.prototype.getType = function() {
        return this.type;
    };
    Msg.Elem.Images.Image.prototype.getSize = function() {
        return this.size;
    };
    Msg.Elem.Images.Image.prototype.getWidth = function() {
        return this.width;
    };
    Msg.Elem.Images.Image.prototype.getHeight = function() {
        return this.height;
    };
    Msg.Elem.Images.Image.prototype.getUrl = function() {
        return this.url;
    };

    // class Msg.Elem.Sound
    Msg.Elem.Sound = function(uuid, second, size, senderId, receiverId, downFlag, chatType) {
        this.uuid = uuid; //文件id
        this.second = second; //时长，单位：秒
        this.size = size; //大小，单位：字节
        this.senderId = senderId; //发送者
        this.receiverId = receiverId; //接收方id
        this.downFlag = downFlag; //下载标志位
        this.busiId = chatType == SESSION_TYPE.C2C ? 2 : 1; //busi_id ( 1：群    2:C2C)

        //根据不同情况拉取数据
        //是否需要申请下载地址  0:到架平申请  1:到cos申请  2:不需要申请, 直接拿url下载
        if (downFlag !== undefined && busiId !== undefined) {
            getFileDownUrlV2(uuid, senderId, second, downFlag, receiverId, this.busiId, UPLOAD_RES_TYPE.SOUND);
        } else {
            this.downUrl = getSoundDownUrl(uuid, senderId, second); //下载地址
        }
    };
    Msg.Elem.Sound.prototype.getUUID = function() {
        return this.uuid;
    };
    Msg.Elem.Sound.prototype.getSecond = function() {
        return this.second;
    };
    Msg.Elem.Sound.prototype.getSize = function() {
        return this.size;
    };
    Msg.Elem.Sound.prototype.getSenderId = function() {
        return this.senderId;
    };
    Msg.Elem.Sound.prototype.getDownUrl = function() {
        return this.downUrl;
    };
    Msg.Elem.Sound.prototype.toHtml = function() {
        if (BROWSER_INFO.type == 'ie' && parseInt(BROWSER_INFO.ver) <= 8) {
            return '[这是一条语音消息]demo暂不支持ie8(含)以下浏览器播放语音,语音URL:' + this.downUrl;
        }
        return '<audio id="uuid_' + this.uuid + '" src="' + this.downUrl + '" controls="controls" onplay="onChangePlayAudio(this)" preload="none"></audio>';
    };

    // class Msg.Elem.File
    Msg.Elem.File = function(uuid, name, size, senderId, receiverId, downFlag, chatType) {
        this.uuid = uuid; //文件id
        this.name = name; //文件名
        this.size = size; //大小，单位：字节
        this.senderId = senderId; //发送者
        this.receiverId = receiverId; //接收方id
        this.downFlag = downFlag; //下载标志位

        this.busiId = chatType == SESSION_TYPE.C2C ? 2 : 1; //busi_id ( 1：群    2:C2C)
        //根据不同情况拉取数据
        //是否需要申请下载地址  0:到架平申请  1:到cos申请  2:不需要申请, 直接拿url下载
        if (downFlag !== undefined && busiId !== undefined) {
            getFileDownUrlV2(uuid, senderId, name, downFlag, receiverId, this.busiId, UPLOAD_RES_TYPE.FILE);
        } else {
            this.downUrl = getFileDownUrl(uuid, senderId, name); //下载地址
        }
    };
    Msg.Elem.File.prototype.getUUID = function() {
        return this.uuid;
    };
    Msg.Elem.File.prototype.getName = function() {
        return this.name;
    };
    Msg.Elem.File.prototype.getSize = function() {
        return this.size;
    };
    Msg.Elem.File.prototype.getSenderId = function() {
        return this.senderId;
    };
    Msg.Elem.File.prototype.getDownUrl = function() {
        return this.downUrl;
    };
    Msg.Elem.File.prototype.getDownFlag = function() {
        return this.downFlag;
    };
    Msg.Elem.File.prototype.toHtml = function() {
        var fileSize, unitStr;
        fileSize = this.size;
        unitStr = "Byte";
        if (this.size >= 1024) {
            fileSize = Math.round(this.size / 1024);
            unitStr = "KB";
        }
        return '<a href="javascript" onclick="webim.onDownFile("' + this.uuid + '")" title="点击下载文件" ><i class="glyphicon glyphicon-file">&nbsp;' + this.name + '(' + fileSize + unitStr + ')</i></a>';
    };

    // class Msg.Elem.GroupTip 群提示消息对象
    Msg.Elem.GroupTip = function(opType, opUserId, groupId, groupName, userIdList) {
        this.opType = opType; //操作类型
        this.opUserId = opUserId; //操作者id
        this.groupId = groupId; //群id
        this.groupName = groupName; //群名称
        this.userIdList = userIdList ? userIdList : []; //被操作的用户id列表
        this.groupInfoList = []; //新的群资料信息，群资料变更时才有值
        this.memberInfoList = []; //新的群成员资料信息，群成员资料变更时才有值
        this.groupMemberNum = null; //群成员数，操作类型为加群或者退群时才有值
    };
    Msg.Elem.GroupTip.prototype.addGroupInfo = function(groupInfo) {
        this.groupInfoList.push(groupInfo);
    };
    Msg.Elem.GroupTip.prototype.addMemberInfo = function(memberInfo) {
        this.memberInfoList.push(memberInfo);
    };
    Msg.Elem.GroupTip.prototype.getOpType = function() {
        return this.opType;
    };
    Msg.Elem.GroupTip.prototype.getOpUserId = function() {
        return this.opUserId;
    };
    Msg.Elem.GroupTip.prototype.getGroupId = function() {
        return this.groupId;
    };
    Msg.Elem.GroupTip.prototype.getGroupName = function() {
        return this.groupName;
    };
    Msg.Elem.GroupTip.prototype.getUserIdList = function() {
        return this.userIdList;
    };
    Msg.Elem.GroupTip.prototype.getGroupInfoList = function() {
        return this.groupInfoList;
    };
    Msg.Elem.GroupTip.prototype.getMemberInfoList = function() {
        return this.memberInfoList;
    };
    Msg.Elem.GroupTip.prototype.getGroupMemberNum = function() {
        return this.groupMemberNum;
    };
    Msg.Elem.GroupTip.prototype.setGroupMemberNum = function(groupMemberNum) {
        return this.groupMemberNum = groupMemberNum;
    };
    Msg.Elem.GroupTip.prototype.toHtml = function() {
        var text = "[群提示消息]";
        var maxIndex = GROUP_TIP_MAX_USER_COUNT - 1;
        switch (this.opType) {
            case GROUP_TIP_TYPE.JOIN: //加入群
                text += this.opUserId + "邀请了";
                for (var m in this.userIdList) {
                    text += this.userIdList[m] + ",";
                    if (this.userIdList.length > GROUP_TIP_MAX_USER_COUNT && m == maxIndex) {
                        text += "等" + this.userIdList.length + "人";
                        break;
                    }
                }
                text += "加入该群";
                break;
            case GROUP_TIP_TYPE.QUIT: //退出群
                text += this.opUserId + "主动退出该群";
                break;
            case GROUP_TIP_TYPE.KICK: //踢出群
                text += this.opUserId + "将";
                for (var m in this.userIdList) {
                    text += this.userIdList[m] + ",";
                    if (this.userIdList.length > GROUP_TIP_MAX_USER_COUNT && m == maxIndex) {
                        text += "等" + this.userIdList.length + "人";
                        break;
                    }
                }
                text += "踢出该群";
                break;
            case GROUP_TIP_TYPE.SET_ADMIN: //设置管理员
                text += this.opUserId + "将";
                for (var m in this.userIdList) {
                    text += this.userIdList[m] + ",";
                    if (this.userIdList.length > GROUP_TIP_MAX_USER_COUNT && m == maxIndex) {
                        text += "等" + this.userIdList.length + "人";
                        break;
                    }
                }
                text += "设为管理员";
                break;
            case GROUP_TIP_TYPE.CANCEL_ADMIN: //取消管理员
                text += this.opUserId + "取消";
                for (var m in this.userIdList) {
                    text += this.userIdList[m] + ",";
                    if (this.userIdList.length > GROUP_TIP_MAX_USER_COUNT && m == maxIndex) {
                        text += "等" + this.userIdList.length + "人";
                        break;
                    }
                }
                text += "的管理员资格";
                break;


            case GROUP_TIP_TYPE.MODIFY_GROUP_INFO: //群资料变更
                text += this.opUserId + "修改了群资料：";
                for (var m in this.groupInfoList) {
                    var type = this.groupInfoList[m].getType();
                    var value = this.groupInfoList[m].getValue();
                    switch (type) {
                        case GROUP_TIP_MODIFY_GROUP_INFO_TYPE.FACE_URL:
                            text += "群头像为" + value + "; ";
                            break;
                        case GROUP_TIP_MODIFY_GROUP_INFO_TYPE.NAME:
                            text += "群名称为" + value + "; ";
                            break;
                        case GROUP_TIP_MODIFY_GROUP_INFO_TYPE.OWNER:
                            text += "群主为" + value + "; ";
                            break;
                        case GROUP_TIP_MODIFY_GROUP_INFO_TYPE.NOTIFICATION:
                            text += "群公告为" + value + "; ";
                            break;
                        case GROUP_TIP_MODIFY_GROUP_INFO_TYPE.INTRODUCTION:
                            text += "群简介为" + value + "; ";
                            break;
                        default:
                            text += "未知信息为:type=" + type + ",value=" + value + "; ";
                            break;
                    }
                }
                break;

            case GROUP_TIP_TYPE.MODIFY_MEMBER_INFO: //群成员资料变更(禁言时间)
                text += this.opUserId + "修改了群成员资料:";
                for (var m in this.memberInfoList) {
                    var userId = this.memberInfoList[m].getUserId();
                    var shutupTime = this.memberInfoList[m].getShutupTime();
                    text += userId + ": ";
                    if (shutupTime != null && shutupTime !== undefined) {
                        if (shutupTime == 0) {
                            text += "取消禁言; ";
                        } else {
                            text += "禁言" + shutupTime + "秒; ";
                        }
                    } else {
                        text += " shutupTime为空";
                    }
                    if (this.memberInfoList.length > GROUP_TIP_MAX_USER_COUNT && m == maxIndex) {
                        text += "等" + this.memberInfoList.length + "人";
                        break;
                    }
                }
                break;

            case GROUP_TIP_TYPE.READED: //消息已读
                /**/
                Log.info("消息已读同步")
                break;
            default:
                text += "未知群提示消息类型：type=" + this.opType;
                break;
        }
        return text;
    };

    // class Msg.Elem.GroupTip.GroupInfo，变更的群资料信息对象
    Msg.Elem.GroupTip.GroupInfo = function(type, value) {
        this.type = type; //群资料信息类型
        this.value = value; //对应的值
    };
    Msg.Elem.GroupTip.GroupInfo.prototype.getType = function() {
        return this.type;
    };
    Msg.Elem.GroupTip.GroupInfo.prototype.getValue = function() {
        return this.value;
    };

    // class Msg.Elem.GroupTip.MemberInfo，变更的群成员资料信息对象
    Msg.Elem.GroupTip.MemberInfo = function(userId, shutupTime) {
        this.userId = userId; //群成员id
        this.shutupTime = shutupTime; //群成员被禁言时间，0表示取消禁言，大于0表示被禁言时长，单位：秒
    };
    Msg.Elem.GroupTip.MemberInfo.prototype.getUserId = function() {
        return this.userId;
    };
    Msg.Elem.GroupTip.MemberInfo.prototype.getShutupTime = function() {
        return this.shutupTime;
    };

    // 自定义消息类型 class Msg.Elem.Custom
    Msg.Elem.Custom = function(data, desc, ext) {
        this.data = data; //数据
        this.desc = desc; //描述
        this.ext = ext; //扩展字段
    };
    Msg.Elem.Custom.prototype.getData = function() {
        return this.data;
    };
    Msg.Elem.Custom.prototype.getDesc = function() {
        return this.desc;
    };
    Msg.Elem.Custom.prototype.getExt = function() {
        return this.ext;
    };
    Msg.Elem.Custom.prototype.toHtml = function() {
        return this.data;
    };

    // singleton object MsgStore
    var MsgStore = new function() {
            var sessMap = {}; //跟所有用户或群的聊天记录MAP
            var sessTimeline = []; //按时间降序排列的会话列表
            window.msgCache = {}; //消息缓存，用于判重
            //C2C
            this.cookie = ""; //上一次拉取新c2c消息的cookie
            this.syncFlag = 0; //上一次拉取新c2c消息的是否继续拉取标记

            var visitSess = function(visitor) {
                for (var i in sessMap) {
                    visitor(sessMap[i]);
                }
            };
            // window.msgCache = msgCache;
            //消息查重
            var checkDupMsg = function(msg) {
                var dup = false;
                var first_key = msg.sess._impl.skey;
                var second_key = msg.isSend + msg.seq + msg.random;
                var tempMsg = msgCache[first_key] && msgCache[first_key][second_key];
                if (tempMsg) {
                    dup = true;
                }
                if (msgCache[first_key]) {
                    msgCache[first_key][second_key] = {
                        time: msg.time
                    };
                } else {
                    msgCache[first_key] = {};
                    msgCache[first_key][second_key] = {
                        time: msg.time
                    };
                }
                return dup;
            };

            this.sessMap = function() {
                return sessMap;
            };
            this.sessCount = function() {
                return sessTimeline.length;
            };
            this.sessByTypeId = function(type, id) {
                var skey = Session.skey(type, id);
                if (skey === undefined || skey == null) return null;
                return sessMap[skey];
            };
            this.delSessByTypeId = function(type, id) {
                var skey = Session.skey(type, id);
                if (skey === undefined || skey == null) return false;
                if (sessMap[skey]) {
                    delete sessMap[skey];
                    delete msgCache[skey];
                }
                return true;
            };
            this.resetCookieAndSyncFlag = function() {
                this.cookie = "";
                this.syncFlag = 0;
            };

            //切换将当前会话的自动读取消息标志为isOn,重置其他会话的自动读取消息标志为false
            this.setAutoRead = function(selSess, isOn, isResetAll) {
                if (isResetAll)
                    visitSess(function(s) {
                        s._impl.isAutoRead = false;
                    });
                if (selSess) {
                    selSess._impl.isAutoRead = isOn; //
                    if (isOn) { //是否调用已读上报接口
                        selSess._impl.unread = 0;

                        if (selSess._impl.type == SESSION_TYPE.C2C) { //私聊消息已读上报
                            var tmpC2CMsgReadedItem = [];
                            tmpC2CMsgReadedItem.push(new C2CMsgReadedItem(selSess._impl.id, selSess._impl.time));
                            //调用C2C消息已读上报接口
                            proto_c2CMsgReaded(MsgStore.cookie,
                                tmpC2CMsgReadedItem,
                                function(resp) {
                                    log.info("[setAutoRead]: c2CMsgReaded success");
                                },
                                function(err) {
                                    log.error("[setAutoRead}: c2CMsgReaded failed:" + err.ErrorInfo);
                                });
                        } else if (selSess._impl.type == SESSION_TYPE.GROUP) { //群聊消息已读上报
                            var tmpOpt = {
                                'GroupId': selSess._impl.id,
                                'MsgReadedSeq': selSess._impl.curMaxMsgSeq
                            };
                            //调用group消息已读上报接口
                            proto_groupMsgReaded(tmpOpt,
                                function(resp) {
                                    log.info("groupMsgReaded success");

                                },
                                function(err) {
                                    log.error("groupMsgReaded failed:" + err.ErrorInfo);

                                });
                        }
                    }
                }
            };

            this.c2CMsgReaded = function(opts, cbOk, cbErr) {
                var tmpC2CMsgReadedItem = [];
                tmpC2CMsgReadedItem.push(new C2CMsgReadedItem(opts.To_Account, opts.LastedMsgTime));
                //调用C2C消息已读上报接口
                proto_c2CMsgReaded(MsgStore.cookie,
                    tmpC2CMsgReadedItem,
                    function(resp) {
                        if (cbOk) {
                            log.info("c2CMsgReaded success");
                            cbOk(resp);
                        }
                    },
                    function(err) {
                        if (cbErr) {
                            log.error("c2CMsgReaded failed:" + err.ErrorInfo);
                            cbErr(err);
                        }
                    });
            };

            this.addSession = function(sess) {
                sessMap[sess._impl.skey] = sess;
            };
            this.delSession = function(sess) {
                delete sessMap[sess._impl.skey];
            };
            this.addMsg = function(msg) {
                if (checkDupMsg(msg)) return false;
                var sess = msg.sess;
                if (!sessMap[sess._impl.skey]) this.addSession(sess);
                sess._impl_addMsg(msg);
                return true;
            };
            this.updateTimeline = function() {
                var arr = new Array;
                visitSess(function(sess) {
                    arr.push(sess);
                });
                arr.sort(function(a, b) {
                    return b.time - a.time;
                });
                sessTimeline = arr;
            };
        };
    // singleton object MsgManager
    var MsgManager = new function() {

            var onMsgCallback = null; //新消息(c2c和group)回调

            var onGroupInfoChangeCallback = null; //群资料变化回调
            //收到新群系统消息回调列表
            var onGroupSystemNotifyCallbacks = {
                "1": null,
                "2": null,
                "3": null,
                "4": null,
                "5": null,
                "6": null,
                "7": null,
                "8": null,
                "9": null,
                "10": null,
                "11": null,
                "15": null,
                "255": null,
                "12": null,
            };
            //监听好友系统通知函数
            var onFriendSystemNotifyCallbacks = {
                "1": null,
                "2": null,
                "3": null,
                "4": null,
                "5": null,
                "6": null,
                "7": null,
                "8": null
            };

            var onProfileSystemNotifyCallbacks = {
                "1": null
            };



            //普通长轮询
            var longPollingOn = false; //是否开启普通长轮询
            var isLongPollingRequesting = false; //是否在长轮询ing
            var notifySeq = 0; //c2c通知seq
            var noticeSeq = 0; //群消息seq

            //大群长轮询
            var onBigGroupMsgCallback = null; //大群消息回调
            var bigGroupLongPollingOn = false; //是否开启长轮询
            var bigGroupLongPollingStartSeq = 0; //请求拉消息的起始seq(大群长轮询)
            var bigGroupLongPollingHoldTime = 90; //客户端长轮询的超时时间，单位是秒(大群长轮询)
            var bigGroupLongPollingKey = null; //客户端加入群组后收到的的Key(大群长轮询)
            var bigGroupLongPollingMsgMap = {}; //记录收到的群消息数
            var onC2cEventCallbacks = {
                "92": null, //消息已读通知,
                "96": null
            };;
            var onKickedEventCall = null; //多实例登录回调
            var onAppliedDownloadUrl = null;


            var getLostGroupMsgCount = 0; //补拉丢失的群消息次数
            //我的群当前最大的seq
            var myGroupMaxSeqs = {}; //用于补拉丢失的群消息

            var groupSystemMsgsCache = {}; //群组系统消息缓存,用于判重

            //设置长轮询开关
            //isOn=true 开启
            //isOn=false 停止
            this.setLongPollingOn = function(isOn) {
                longPollingOn = isOn;
            };
            this.getLongPollingOn = function() {
                return longPollingOn;
            };

            //重置长轮询变量
            this.resetLongPollingInfo = function() {
                longPollingOn = false;
                notifySeq = 0;
                noticeSeq = 0;
            };

            //设置大群长轮询开关
            //isOn=true 开启
            //isOn=false 停止
            this.setBigGroupLongPollingOn = function(isOn) {
                bigGroupLongPollingOn = isOn;
            };
            //设置大群长轮询key
            this.setBigGroupLongPollingKey = function(key) {
                bigGroupLongPollingKey = key;
            };
            //重置大群长轮询变量
            this.resetBigGroupLongPollingInfo = function() {
                bigGroupLongPollingOn = false;
                bigGroupLongPollingStartSeq = 0;
                bigGroupLongPollingKey = null;
                bigGroupLongPollingMsgMap = {};
            };

            //设置群消息数据条数
            this.setBigGroupLongPollingMsgMap = function(groupId, count) {
                var bigGroupLongPollingMsgCount = bigGroupLongPollingMsgMap[groupId];
                if (bigGroupLongPollingMsgCount) {
                    bigGroupLongPollingMsgCount = parseInt(bigGroupLongPollingMsgCount) + count;
                    bigGroupLongPollingMsgMap[groupId] = bigGroupLongPollingMsgCount;
                } else {
                    bigGroupLongPollingMsgMap[groupId] = count;
                }
            };

            //重置
            this.clear = function() {

                onGroupInfoChangeCallback = null;
                onGroupSystemNotifyCallbacks = {
                    "1": null, //申请加群请求（只有管理员会收到）
                    "2": null, //申请加群被同意（只有申请人能够收到）
                    "3": null, //申请加群被拒绝（只有申请人能够收到）
                    "4": null, //被管理员踢出群(只有被踢者接收到)
                    "5": null, //群被解散(全员接收)
                    "6": null, //创建群(创建者接收)
                    "7": null, //邀请加群(被邀请者接收)
                    "8": null, //主动退群(主动退出者接收)
                    "9": null, //设置管理员(被设置者接收)
                    "10": null, //取消管理员(被取消者接收)
                    "11": null, //群已被回收(全员接收)
                    "15": null, //群已被回收(全员接收)
                    "255": null, //用户自定义通知(默认全员接收)
                    "12": null, //邀请加群(被邀请者需要同意)
                };
                onFriendSystemNotifyCallbacks = {
                    "1": null, //好友表增加
                    "2": null, //好友表删除
                    "3": null, //未决增加
                    "4": null, //未决删除
                    "5": null, //黑名单增加
                    "6": null, //黑名单删除
                    "7": null, //未决已读上报
                    "8": null //好友信息(备注，分组)变更
                };
                onProfileSystemNotifyCallbacks = {
                    "1": null //资料修改
                };
                //重置普通长轮询参数
                onMsgCallback = null;
                longPollingOn = false;
                notifySeq = 0; //c2c新消息通知seq
                noticeSeq = 0; //group新消息seq

                //重置大群长轮询参数
                onBigGroupMsgCallback = null;
                bigGroupLongPollingOn = false;
                bigGroupLongPollingStartSeq = 0;
                bigGroupLongPollingKey = null;
                bigGroupLongPollingMsgMap = {};

                groupSystemMsgsCache = {};

                ipList = []; //文件下载地址
                authkey = null; //文件下载票据
                expireTime = null; //票据超时时间
            };

            //初始化文件下载ip和票据
            var initIpAndAuthkey = function(cbOk, cbErr) {
                proto_getIpAndAuthkey(function(resp) {
                        ipList = resp.IpList;
                        authkey = resp.AuthKey;
                        expireTime = resp.ExpireTime;
                        if (cbOk) cbOk(resp);
                    },
                    function(err) {
                        log.error("initIpAndAuthkey failed:" + err.ErrorInfo);
                        if (cbErr) cbErr(err);
                    }
                );
            };

            //初始化我的群当前最大的seq，用于补拉丢失的群消息
            var initMyGroupMaxSeqs = function(cbOk, cbErr) {
                var opts = {
                    'Member_Account': ctx.identifier,
                    'Limit': 1000,
                    'Offset': 0,
                    'GroupBaseInfoFilter': [
                        'NextMsgSeq'
                    ]
                };
                proto_getJoinedGroupListHigh(opts, function(resp) {
                        if (!resp.GroupIdList || resp.GroupIdList.length == 0) {
                            log.info("initMyGroupMaxSeqs: 目前还没有加入任何群组");
                            if (cbOk) cbOk(resp);
                            return;
                        }
                        for (var i = 0; i < resp.GroupIdList.length; i++) {
                            var group_id = resp.GroupIdList[i].GroupId;
                            var curMaxSeq = resp.GroupIdList[i].NextMsgSeq - 1;
                            myGroupMaxSeqs[group_id] = curMaxSeq;
                        }

                        if (cbOk) cbOk(resp);

                    },
                    function(err) {
                        log.error("initMyGroupMaxSeqs failed:" + err.ErrorInfo);
                        if (cbErr) cbErr(err);
                    }
                );
            };

            //补拉群消息
            var getLostGroupMsgs = function(groupId, reqMsgSeq, reqMsgNumber) {
                getLostGroupMsgCount++;
                //发起一个拉群群消息请求
                var tempOpts = {
                    'GroupId': groupId,
                    'ReqMsgSeq': reqMsgSeq,
                    'ReqMsgNumber': reqMsgNumber
                };
                //发起一个拉群群消息请求
                log.warn("第" + getLostGroupMsgCount + "次补齐群消息,参数=" + JSON.stringify(tempOpts));
                MsgManager.syncGroupMsgs(tempOpts);
            };

            //更新群当前最大消息seq
            var updateMyGroupCurMaxSeq = function(groupId, msgSeq) {
                //更新myGroupMaxSeqs中的群最大seq
                var curMsgSeq = myGroupMaxSeqs[groupId]
                if (curMsgSeq) { //如果存在，比较大小
                    if (msgSeq > curMsgSeq) {
                        myGroupMaxSeqs[groupId] = msgSeq;
                    }
                } else { //不存在，新增
                    myGroupMaxSeqs[groupId] = msgSeq;
                }
            };

            //添加群消息列表
            var addGroupMsgList = function(msgs, new_group_msgs) {
                for (var p in msgs) {
                    var newGroupMsg = msgs[p];
                    //发群消息时，长轮询接口会返回用户自己发的群消息
                    //if(newGroupMsg.From_Account && newGroupMsg.From_Account!=ctx.identifier ){
                    if (newGroupMsg.From_Account) {
                        //false-不是主动拉取的历史消息
                        //true-需要保存到sdk本地session,并且需要判重
                        var msg = handlerGroupMsg(newGroupMsg, false, true);
                        if (msg) { //不为空，加到新消息里
                            new_group_msgs.push(msg);
                        }
                        //更新myGroupMaxSeqs中的群最大seq
                        updateMyGroupCurMaxSeq(newGroupMsg.ToGroupId, newGroupMsg.MsgSeq);
                    }
                }
                return new_group_msgs;
            };

            //处理收到的群普通和提示消息
            var handlerOrdinaryAndTipC2cMsgs = function(eventType, groupMsgArray) {
                var groupMsgMap = {}; //保存收到的C2c消息信息（群号，最小，最大消息seq，消息列表）
                var new_group_msgs = [];
                var minGroupMsgSeq = 99999999;
                var maxGroupMsgSeq = -1;
                for (var j in groupMsgArray) {

                    var groupMsgs = groupMsgMap[groupMsgArray[j].ToGroupId];
                    if (!groupMsgs) {
                        groupMsgs = groupMsgMap[groupMsgArray[j].ToGroupId] = {
                            "min": minGroupMsgSeq, //收到新消息最小seq
                            "max": maxGroupMsgSeq, //收到新消息最大seq
                            "msgs": [] //收到的新消息
                        };
                    }
                    //更新长轮询的群NoticeSeq
                    if (groupMsgArray[j].NoticeSeq > noticeSeq) {
                        log.warn("noticeSeq=" + noticeSeq + ",msgNoticeSeq=" + groupMsgArray[j].NoticeSeq);
                        noticeSeq = groupMsgArray[j].NoticeSeq;
                    }
                    groupMsgArray[j].Event = eventType;
                    groupMsgMap[groupMsgArray[j].ToGroupId].msgs.push(groupMsgArray[j]); //新增一条消息
                    if (groupMsgArray[j].MsgSeq < groupMsgs.min) { //记录最小的消息seq
                        groupMsgMap[groupMsgArray[j].ToGroupId].min = groupMsgArray[j].MsgSeq;
                    }
                    if (groupMsgArray[j].MsgSeq > groupMsgs.max) { //记录最大的消息seq
                        groupMsgMap[groupMsgArray[j].ToGroupId].max = groupMsgArray[j].MsgSeq;
                    }
                }

                for (var groupId in groupMsgMap) {
                    var tempCount = groupMsgMap[groupId].max - groupMsgMap[groupId].min + 1; //收到的新的群消息数
                    var curMaxMsgSeq = myGroupMaxSeqs[groupId]; //获取本地保存的群最大消息seq
                    if (curMaxMsgSeq) { //存在这个群的最大消息seq
                        //高并发情况下，长轮询可能存在丢消息，这时需要客户端通过拉取群消息接口补齐下
                        //1、如果收到的新消息最小seq比当前最大群消息seq大于1，则表示收到的群消息发生跳跃，需要补齐
                        //2、收到的新群消息seq存在不连续情况，也需要补齐
                        if (groupMsgMap[groupId].min - curMaxMsgSeq > 1 || groupMsgMap[groupId].msgs.length < tempCount) {
                            //发起一个拉群群消息请求
                            log.warn("发起一次补齐群消息请求,curMaxMsgSeq=" + curMaxMsgSeq + ", minMsgSeq=" + groupMsgMap[groupId].min + ", maxMsgSeq=" + groupMsgMap[groupId].max + ", msgs.length=" + groupMsgMap[groupId].msgs.length + ", tempCount=" + tempCount);
                            getLostGroupMsgs(groupId, groupMsgMap[groupId].max, groupMsgMap[groupId].max - curMaxMsgSeq);
                            //更新myGroupMaxSeqs中的群最大seq
                            updateMyGroupCurMaxSeq(groupId, groupMsgMap[groupId].max);
                        } else {
                            new_group_msgs = addGroupMsgList(groupMsgMap[groupId].msgs, new_group_msgs);
                        }
                    } else { //不存在该群的最大消息seq
                        log.warn("不存在该群的最大消息seq，群id=" + groupId);
                        //高并发情况下，长轮询可能存在丢消息，这时需要客户端通过拉取群消息接口补齐下
                        //1、收到的新群消息seq存在不连续情况，也需要补齐
                        if (groupMsgMap[groupId].msgs.length < tempCount) {
                            //发起一个拉群群消息请求
                            log.warn("发起一次补齐群消息请求,minMsgSeq=" + groupMsgMap[groupId].min + ", maxMsgSeq=" + groupMsgMap[groupId].max + ", msgs.length=" + groupMsgMap[groupId].msgs.length + ", tempCount=" + tempCount);
                            getLostGroupMsgs(groupId, groupMsgMap[groupId].max, tempCount);
                            //更新myGroupMaxSeqs中的群最大seq
                            updateMyGroupCurMaxSeq(groupId, groupMsgMap[groupId].max);
                        } else {
                            new_group_msgs = addGroupMsgList(groupMsgMap[groupId].msgs, new_group_msgs);
                        }
                    }
                }
                if (new_group_msgs.length) {
                    MsgStore.updateTimeline();
                }
                if (onMsgCallback && new_group_msgs.length) onMsgCallback(new_group_msgs);

            };

            //处理收到的群普通和提示消息
            var handlerOrdinaryAndTipGroupMsgs = function(eventType, groupMsgArray) {
                var groupMsgMap = {}; //保存收到的群消息信息（群号，最小，最大消息seq，消息列表）
                var new_group_msgs = [];
                var minGroupMsgSeq = 99999999;
                var maxGroupMsgSeq = -1;
                for (var j in groupMsgArray) {

                    var groupMsgs = groupMsgMap[groupMsgArray[j].ToGroupId];
                    if (!groupMsgs) {
                        groupMsgs = groupMsgMap[groupMsgArray[j].ToGroupId] = {
                            "min": minGroupMsgSeq, //收到新消息最小seq
                            "max": maxGroupMsgSeq, //收到新消息最大seq
                            "msgs": [] //收到的新消息
                        };
                    }
                    //更新长轮询的群NoticeSeq
                    if (groupMsgArray[j].NoticeSeq > noticeSeq) {
                        log.warn("noticeSeq=" + noticeSeq + ",msgNoticeSeq=" + groupMsgArray[j].NoticeSeq);
                        noticeSeq = groupMsgArray[j].NoticeSeq;
                    }
                    groupMsgArray[j].Event = eventType;
                    groupMsgMap[groupMsgArray[j].ToGroupId].msgs.push(groupMsgArray[j]); //新增一条消息
                    if (groupMsgArray[j].MsgSeq < groupMsgs.min) { //记录最小的消息seq
                        groupMsgMap[groupMsgArray[j].ToGroupId].min = groupMsgArray[j].MsgSeq;
                    }
                    if (groupMsgArray[j].MsgSeq > groupMsgs.max) { //记录最大的消息seq
                        groupMsgMap[groupMsgArray[j].ToGroupId].max = groupMsgArray[j].MsgSeq;
                    }
                }

                for (var groupId in groupMsgMap) {
                    var tempCount = groupMsgMap[groupId].max - groupMsgMap[groupId].min + 1; //收到的新的群消息数
                    var curMaxMsgSeq = myGroupMaxSeqs[groupId]; //获取本地保存的群最大消息seq
                    if (curMaxMsgSeq) { //存在这个群的最大消息seq
                        //高并发情况下，长轮询可能存在丢消息，这时需要客户端通过拉取群消息接口补齐下
                        //1、如果收到的新消息最小seq比当前最大群消息seq大于1，则表示收到的群消息发生跳跃，需要补齐
                        //2、收到的新群消息seq存在不连续情况，也需要补齐
                        if (groupMsgMap[groupId].min - curMaxMsgSeq > 1 || groupMsgMap[groupId].msgs.length < tempCount) {
                            //发起一个拉群群消息请求
                            log.warn("发起一次补齐群消息请求,curMaxMsgSeq=" + curMaxMsgSeq + ", minMsgSeq=" + groupMsgMap[groupId].min + ", maxMsgSeq=" + groupMsgMap[groupId].max + ", msgs.length=" + groupMsgMap[groupId].msgs.length + ", tempCount=" + tempCount);
                            getLostGroupMsgs(groupId, groupMsgMap[groupId].max, groupMsgMap[groupId].max - curMaxMsgSeq);
                            //更新myGroupMaxSeqs中的群最大seq
                            updateMyGroupCurMaxSeq(groupId, groupMsgMap[groupId].max);
                        } else {
                            new_group_msgs = addGroupMsgList(groupMsgMap[groupId].msgs, new_group_msgs);
                        }
                    } else { //不存在该群的最大消息seq
                        log.warn("不存在该群的最大消息seq，群id=" + groupId);
                        //高并发情况下，长轮询可能存在丢消息，这时需要客户端通过拉取群消息接口补齐下
                        //1、收到的新群消息seq存在不连续情况，也需要补齐
                        if (groupMsgMap[groupId].msgs.length < tempCount) {
                            //发起一个拉群群消息请求
                            log.warn("发起一次补齐群消息请求,minMsgSeq=" + groupMsgMap[groupId].min + ", maxMsgSeq=" + groupMsgMap[groupId].max + ", msgs.length=" + groupMsgMap[groupId].msgs.length + ", tempCount=" + tempCount);
                            getLostGroupMsgs(groupId, groupMsgMap[groupId].max, tempCount);
                            //更新myGroupMaxSeqs中的群最大seq
                            updateMyGroupCurMaxSeq(groupId, groupMsgMap[groupId].max);
                        } else {
                            new_group_msgs = addGroupMsgList(groupMsgMap[groupId].msgs, new_group_msgs);
                        }
                    }
                }
                if (new_group_msgs.length) {
                    MsgStore.updateTimeline();
                }
                if (onMsgCallback && new_group_msgs.length) onMsgCallback(new_group_msgs);

            };

            //处理新的群提示消息
            var handlerGroupTips = function(groupTips) {
                var new_group_msgs = [];
                for (var o in groupTips) {
                    var groupTip = groupTips[o];
                    //添加event字段
                    groupTip.Event = LONG_POLLINNG_EVENT_TYPE.GROUP_TIP;
                    //更新群消息通知seq
                    if (groupTip.NoticeSeq > noticeSeq) {
                        noticeSeq = groupTip.NoticeSeq;
                    }
                    var msg = handlerGroupMsg(groupTip, false, true);
                    if (msg) {
                        new_group_msgs.push(msg);
                    }
                }
                if (new_group_msgs.length) {
                    MsgStore.updateTimeline();
                }
                if (onMsgCallback && new_group_msgs.length) onMsgCallback(new_group_msgs);
            };

            //处理新的群系统消息
            //isNeedValidRepeatMsg 是否需要判重
            var handlerGroupSystemMsgs = function(groupSystemMsgs, isNeedValidRepeatMsg) {
                for (var k in groupSystemMsgs) {
                    var groupTip = groupSystemMsgs[k];
                    var groupReportTypeMsg = groupTip.MsgBody;
                    var reportType = groupReportTypeMsg.ReportType;
                    //当长轮询返回的群系统消息，才需要更新群消息通知seq
                    if (isNeedValidRepeatMsg == false && groupTip.NoticeSeq && groupTip.NoticeSeq > noticeSeq) {
                        noticeSeq = groupTip.NoticeSeq;
                    }
                    var toAccount = groupTip.GroupInfo.To_Account;
                    //过滤本不应该给自己的系统消息
                    /*if (!toAccount || toAccount != ctx.identifier) {
                 log.error("收到本不应该给自己的系统消息: To_Account=" + toAccount);
                 continue;
                 }*/
                    if (isNeedValidRepeatMsg) {
                        //var key=groupTip.ToGroupId+"_"+reportType+"_"+groupTip.MsgTimeStamp+"_"+groupReportTypeMsg.Operator_Account;
                        var key = groupTip.ToGroupId + "_" + reportType + "_" + groupReportTypeMsg.Operator_Account;
                        var isExist = groupSystemMsgsCache[key];
                        if (isExist) {
                            log.warn("收到重复的群系统消息：key=" + key);
                            continue;
                        }
                        groupSystemMsgsCache[key] = true;
                    }

                    var notify = {
                        "SrcFlag": 0,
                        "ReportType": reportType,
                        "GroupId": groupTip.ToGroupId,
                        "GroupName": groupTip.GroupInfo.GroupName,
                        "Operator_Account": groupReportTypeMsg.Operator_Account,
                        "MsgTime": groupTip.MsgTimeStamp,
                        "groupReportTypeMsg": groupReportTypeMsg
                    };
                    switch (reportType) {
                        case GROUP_SYSTEM_TYPE.JOIN_GROUP_REQUEST: //申请加群(只有管理员会接收到)
                            notify["RemarkInfo"] = groupReportTypeMsg.RemarkInfo;
                            notify["MsgKey"] = groupReportTypeMsg.MsgKey;
                            notify["Authentication"] = groupReportTypeMsg.Authentication;
                            notify["UserDefinedField"] = groupTip.UserDefinedField;
                            notify["From_Account"] = groupTip.From_Account;
                            notify["MsgSeq"] = groupTip.ClientSeq;
                            notify["MsgRandom"] = groupTip.MsgRandom;
                            break;
                        case GROUP_SYSTEM_TYPE.JOIN_GROUP_ACCEPT: //申请加群被同意(只有申请人自己接收到)
                        case GROUP_SYSTEM_TYPE.JOIN_GROUP_REFUSE: //申请加群被拒绝(只有申请人自己接收到)
                            notify["RemarkInfo"] = groupReportTypeMsg.RemarkInfo;
                            break;
                        case GROUP_SYSTEM_TYPE.KICK: //被管理员踢出群(只有被踢者接收到)
                        case GROUP_SYSTEM_TYPE.DESTORY: //群被解散(全员接收)
                        case GROUP_SYSTEM_TYPE.CREATE: //创建群(创建者接收, 不展示)
                        case GROUP_SYSTEM_TYPE.INVITED_JOIN_GROUP_REQUEST: //邀请加群(被邀请者接收)
                        case GROUP_SYSTEM_TYPE.INVITED_JOIN_GROUP_REQUEST_AGREE: //邀请加群(被邀请者需同意)
                        case GROUP_SYSTEM_TYPE.QUIT: //主动退群(主动退出者接收, 不展示)
                        case GROUP_SYSTEM_TYPE.SET_ADMIN: //群设置管理员(被设置者接收)
                        case GROUP_SYSTEM_TYPE.CANCEL_ADMIN: //取消管理员(被取消者接收)
                        case GROUP_SYSTEM_TYPE.REVOKE: //群已被回收(全员接收, 不展示)
                            break;
                        case GROUP_SYSTEM_TYPE.READED: //群消息已读同步
                            break;
                        case GROUP_SYSTEM_TYPE.CUSTOM: //用户自定义通知(默认全员接收)
                            notify["MsgSeq"] = groupTip.MsgSeq;
                            notify["UserDefinedField"] = groupReportTypeMsg.UserDefinedField;
                            break;
                        default:
                            log.error("未知群系统消息类型：reportType=" + reportType);
                            break;
                    }

                    if (isNeedValidRepeatMsg) {
                        //注释只收取一种通知
                        if (reportType == GROUP_SYSTEM_TYPE.JOIN_GROUP_REQUEST) {
                            //回调
                            if (onGroupSystemNotifyCallbacks[reportType]) {
                                onGroupSystemNotifyCallbacks[reportType](notify);
                            } else {
                                log.error("未知群系统消息类型：reportType=" + reportType);
                            }
                        }
                    } else {
                        //回调
                        if (onGroupSystemNotifyCallbacks[reportType]) {
                            if (reportType == GROUP_SYSTEM_TYPE.READED) {
                                var arr = notify.groupReportTypeMsg.GroupReadInfoArray;
                                for (var i = 0, l = arr.length; i < l; i++) {
                                    var item = arr[i];
                                    onGroupSystemNotifyCallbacks[reportType](item);
                                }
                            } else {
                                onGroupSystemNotifyCallbacks[reportType](notify);
                            }
                        }
                    }
                } //loop
            };


            //处理新的好友系统通知
            //isNeedValidRepeatMsg 是否需要判重
            var handlerFriendSystemNotices = function(friendSystemNotices, isNeedValidRepeatMsg) {
                var friendNotice, type, notify;
                for (var k in friendSystemNotices) {
                    friendNotice = friendSystemNotices[k];
                    type = friendNotice.PushType;
                    //当长轮询返回的群系统消息，才需要更新通知seq
                    if (isNeedValidRepeatMsg == false && friendNotice.NoticeSeq && friendNotice.NoticeSeq > noticeSeq) {
                        noticeSeq = friendNotice.NoticeSeq;
                    }
                    notify = {
                        'Type': type
                    };
                    switch (type) {
                        case FRIEND_NOTICE_TYPE.FRIEND_ADD: //好友表增加
                            notify["Accounts"] = friendNotice.FriendAdd_Account;
                            break;
                        case FRIEND_NOTICE_TYPE.FRIEND_DELETE: //好友表删除
                            notify["Accounts"] = friendNotice.FriendDel_Account;
                            break;
                        case FRIEND_NOTICE_TYPE.PENDENCY_ADD: //未决增加
                            notify["PendencyList"] = friendNotice.PendencyAdd;
                            break;
                        case FRIEND_NOTICE_TYPE.PENDENCY_DELETE: //未决删除
                            notify["Accounts"] = friendNotice.FrienPencydDel_Account;
                            break;
                        case FRIEND_NOTICE_TYPE.BLACK_LIST_ADD: //黑名单增加
                            notify["Accounts"] = friendNotice.BlackListAdd_Account;
                            break;
                        case FRIEND_NOTICE_TYPE.BLACK_LIST_DELETE: //黑名单删除
                            notify["Accounts"] = friendNotice.BlackListDel_Account;
                            break;
                            /*case FRIEND_NOTICE_TYPE.PENDENCY_REPORT://未决已读上报

                     break;
                     case FRIEND_NOTICE_TYPE.FRIEND_UPDATE://好友数据更新

                     break;
                     */
                        default:
                            log.error("未知好友系统通知类型：friendNotice=" + JSON.stringify(friendNotice));
                            break;
                    }

                    if (isNeedValidRepeatMsg) {
                        if (type == FRIEND_NOTICE_TYPE.PENDENCY_ADD) {
                            //回调
                            if (onFriendSystemNotifyCallbacks[type]) onFriendSystemNotifyCallbacks[type](notify);
                        }
                    } else {
                        //回调
                        if (onFriendSystemNotifyCallbacks[type]) onFriendSystemNotifyCallbacks[type](notify);
                    }
                } //loop
            };

            //处理新的资料系统通知
            //isNeedValidRepeatMsg 是否需要判重
            var handlerProfileSystemNotices = function(profileSystemNotices, isNeedValidRepeatMsg) {
                var profileNotice, type, notify;
                for (var k in profileSystemNotices) {
                    profileNotice = profileSystemNotices[k];
                    type = profileNotice.PushType;
                    //当长轮询返回的群系统消息，才需要更新通知seq
                    if (isNeedValidRepeatMsg == false && profileNotice.NoticeSeq && profileNotice.NoticeSeq > noticeSeq) {
                        noticeSeq = profileNotice.NoticeSeq;
                    }
                    notify = {
                        'Type': type
                    };
                    switch (type) {
                        case PROFILE_NOTICE_TYPE.PROFILE_MODIFY: //资料修改
                            notify["Profile_Account"] = profileNotice.Profile_Account;
                            notify["ProfileList"] = profileNotice.ProfileList;
                            break;
                        default:
                            log.error("未知资料系统通知类型：profileNotice=" + JSON.stringify(profileNotice));
                            break;
                    }

                    if (isNeedValidRepeatMsg) {
                        if (type == PROFILE_NOTICE_TYPE.PROFILE_MODIFY) {
                            //回调
                            if (onProfileSystemNotifyCallbacks[type]) onProfileSystemNotifyCallbacks[type](notify);
                        }
                    } else {
                        //回调
                        if (onProfileSystemNotifyCallbacks[type]) onProfileSystemNotifyCallbacks[type](notify);
                    }
                } //loop
            };

            //处理新的群系统消息(用于直播大群长轮询)
            var handlerGroupSystemMsg = function(groupTip) {
                var groupReportTypeMsg = groupTip.MsgBody;
                var reportType = groupReportTypeMsg.ReportType;
                var toAccount = groupTip.GroupInfo.To_Account;
                //过滤本不应该给自己的系统消息
                //if(!toAccount || toAccount!=ctx.identifier){
                //    log.error("收到本不应该给自己的系统消息: To_Account="+toAccount);
                //    continue;
                //}
                var notify = {
                    "SrcFlag": 1,
                    "ReportType": reportType,
                    "GroupId": groupTip.ToGroupId,
                    "GroupName": groupTip.GroupInfo.GroupName,
                    "Operator_Account": groupReportTypeMsg.Operator_Account,
                    "MsgTime": groupTip.MsgTimeStamp
                };
                switch (reportType) {
                    case GROUP_SYSTEM_TYPE.JOIN_GROUP_REQUEST: //申请加群(只有管理员会接收到)
                        notify["RemarkInfo"] = groupReportTypeMsg.RemarkInfo;
                        notify["MsgKey"] = groupReportTypeMsg.MsgKey;
                        notify["Authentication"] = groupReportTypeMsg.Authentication;
                        notify["UserDefinedField"] = groupTip.UserDefinedField;
                        notify["From_Account"] = groupTip.From_Account;
                        notify["MsgSeq"] = groupTip.ClientSeq;
                        notify["MsgRandom"] = groupTip.MsgRandom;
                        break;
                    case GROUP_SYSTEM_TYPE.JOIN_GROUP_ACCEPT: //申请加群被同意(只有申请人自己接收到)
                    case GROUP_SYSTEM_TYPE.JOIN_GROUP_REFUSE: //申请加群被拒绝(只有申请人自己接收到)
                        notify["RemarkInfo"] = groupReportTypeMsg.RemarkInfo;
                        break;
                    case GROUP_SYSTEM_TYPE.KICK: //被管理员踢出群(只有被踢者接收到)
                    case GROUP_SYSTEM_TYPE.DESTORY: //群被解散(全员接收)
                    case GROUP_SYSTEM_TYPE.CREATE: //创建群(创建者接收, 不展示)
                    case GROUP_SYSTEM_TYPE.INVITED_JOIN_GROUP_REQUEST: //邀请加群(被邀请者接收)
                    case GROUP_SYSTEM_TYPE.INVITED_JOIN_GROUP_REQUEST_AGREE: //邀请加群(被邀请者需要同意)
                    case GROUP_SYSTEM_TYPE.QUIT: //主动退群(主动退出者接收, 不展示)
                    case GROUP_SYSTEM_TYPE.SET_ADMIN: //群设置管理员(被设置者接收)
                    case GROUP_SYSTEM_TYPE.CANCEL_ADMIN: //取消管理员(被取消者接收)
                    case GROUP_SYSTEM_TYPE.REVOKE: //群已被回收(全员接收, 不展示)
                        break;
                    case GROUP_SYSTEM_TYPE.CUSTOM: //用户自定义通知(默认全员接收)
                        notify["MsgSeq"] = groupTip.MsgSeq;
                        notify["UserDefinedField"] = groupReportTypeMsg.UserDefinedField;
                        break;
                    default:
                        log.error("未知群系统消息类型：reportType=" + reportType);
                        break;
                }
                //回调
                if (onGroupSystemNotifyCallbacks[reportType]) onGroupSystemNotifyCallbacks[reportType](notify);

            };

            //处理C2C EVENT 消息通道Array
            var handlerC2cNotifyMsgArray = function(arr) {
                for (var i = 0, l = arr.length; i < l; i++) {
                    handlerC2cEventMsg(arr[i]);
                }
            }

            //处理C2C EVENT 消息通道Item
            var handlerC2cEventMsg = function(notify) {
                var subType = notify.SubMsgType;
                switch (subType) {
                    case C2C_EVENT_SUB_TYPE.READED: //已读通知
                        // stopPolling = true;
                        //回调onMsgReadCallback
                        if (notify.ReadC2cMsgNotify.UinPairReadArray && onC2cEventCallbacks[subType]) {
                            for (var i = 0, l = notify.ReadC2cMsgNotify.UinPairReadArray.length; i < l; i++) {
                                var item = notify.ReadC2cMsgNotify.UinPairReadArray[i];
                                onC2cEventCallbacks[subType](item);
                            }
                        }
                        break;
                    case C2C_EVENT_SUB_TYPE.KICKEDOUT: //已读通知
                        if (onC2cEventCallbacks[subType]) {
                            onC2cEventCallbacks[subType]();
                        }
                        break;
                    default:
                        log.error("未知C2c系统消息：subType=" + subType);
                        break;
                }

            };

            //长轮询
            this.longPolling = function(cbOk, cbErr) {


                var opts = {
                    'Timeout': longPollingDefaultTimeOut / 1000,
                    'Cookie': {
                        'NotifySeq': notifySeq,
                        'NoticeSeq': noticeSeq
                    }
                };
                if (LongPollingId) {
                    opts.Cookie.LongPollingId = LongPollingId;
                    doPolling();
                } else {
                    proto_getLongPollingId({}, function(resp) {
                        LongPollingId = opts.Cookie.LongPollingId = resp.LongPollingId;
                        //根据回包设置超时时间，超时时长不能>60秒，因为webkit手机端的最长超时时间不能大于60s
                        longPollingDefaultTimeOut = resp.Timeout > 60 ? longPollingDefaultTimeOut : resp.Timeout * 1000;
                        doPolling();
                    });
                }

                function doPolling() {
                    proto_longPolling(opts, function(resp) {
                        for (var i in resp.EventArray) {
                            var e = resp.EventArray[i];
                            switch (e.Event) {
                                case LONG_POLLINNG_EVENT_TYPE.C2C: //c2c消息通知
                                    //更新C2C消息通知seq
                                    notifySeq = e.NotifySeq;
                                    log.warn("longpolling: received new c2c msg");
                                    //获取新消息
                                    MsgManager.syncMsgs();
                                    break;
                                case LONG_POLLINNG_EVENT_TYPE.GROUP_COMMON: //普通群消息通知
                                    log.warn("longpolling: received new group msgs");
                                    handlerOrdinaryAndTipGroupMsgs(e.Event, e.GroupMsgArray);
                                    break;
                                case LONG_POLLINNG_EVENT_TYPE.GROUP_TIP: //（全员广播）群提示消息
                                    log.warn("longpolling: received new group tips");
                                    handlerOrdinaryAndTipGroupMsgs(e.Event, e.GroupTips);
                                    break;
                                case LONG_POLLINNG_EVENT_TYPE.GROUP_TIP2: //群提示消息
                                    log.warn("longpolling: received new group tips");
                                    handlerOrdinaryAndTipGroupMsgs(e.Event, e.GroupTips);
                                    break;
                                case LONG_POLLINNG_EVENT_TYPE.GROUP_SYSTEM: //（多终端同步）群系统消息
                                    log.warn("longpolling: received new group system msgs");
                                    //false 表示 通过长轮询收到的群系统消息，可以不判重
                                    handlerGroupSystemMsgs(e.GroupTips, false);
                                    break;
                                case LONG_POLLINNG_EVENT_TYPE.FRIEND_NOTICE: //好友系统通知
                                    log.warn("longpolling: received new friend system notice");
                                    //false 表示 通过长轮询收到的好友系统通知，可以不判重
                                    handlerFriendSystemNotices(e.FriendListMod, false);
                                    break;
                                case LONG_POLLINNG_EVENT_TYPE.PROFILE_NOTICE: //资料系统通知
                                    log.warn("longpolling: received new profile system notice");
                                    //false 表示 通过长轮询收到的资料系统通知，可以不判重
                                    handlerProfileSystemNotices(e.ProfileDataMod, false);
                                    break;
                                case LONG_POLLINNG_EVENT_TYPE.C2C_COMMON: //c2c消息通知
                                    noticeSeq = e.C2cMsgArray[0].NoticeSeq;
                                    //更新C2C消息通知seq
                                    log.warn("longpolling: received new c2c_common msg", noticeSeq);
                                    handlerOrdinaryAndTipC2cMsgs(e.Event, e.C2cMsgArray);
                                    break;
                                case LONG_POLLINNG_EVENT_TYPE.C2C_EVENT: //c2c已读消息通知
                                    noticeSeq = e.C2cNotifyMsgArray[0].NoticeSeq;
                                    log.warn("longpolling: received new c2c_event msg");
                                    handlerC2cNotifyMsgArray(e.C2cNotifyMsgArray);
                                    break;
                                default:
                                    log.error("longpolling收到未知新消息类型: Event=" + e.Event);
                                    break;
                            }
                        }
                        var successInfo = {
                            'ActionStatus': ACTION_STATUS.OK,
                            'ErrorCode': 0
                        };
                        updatecLongPollingStatus(successInfo);
                    }, function(err) {
                        //log.error(err);
                        updatecLongPollingStatus(err);
                        if (cbErr) cbErr(err);
                    });
                }
            };


            //大群 长轮询
            this.bigGroupLongPolling = function(cbOk, cbErr) {

                var opts = {
                    'StartSeq': bigGroupLongPollingStartSeq, //请求拉消息的起始seq
                    'HoldTime': bigGroupLongPollingHoldTime, //客户端长轮询的超时时间，单位是秒
                    'Key': bigGroupLongPollingKey //客户端加入群组后收到的的Key
                };

                proto_bigGroupLongPolling(opts, function(resp) {

                    var msgObjList = [];
                    bigGroupLongPollingStartSeq = resp.NextSeq;
                    bigGroupLongPollingHoldTime = resp.HoldTime;
                    bigGroupLongPollingKey = resp.Key;

                    if (resp.RspMsgList && resp.RspMsgList.length > 0) {
                        var msgCount = 0,
                            msgInfo, event, msg;
                        for (var i = resp.RspMsgList.length - 1; i >= 0; i--) {
                            msgInfo = resp.RspMsgList[i];
                            //如果是已经删除的消息或者发送者帐号为空或者消息内容为空
                            //IsPlaceMsg=1
                            if (msgInfo.IsPlaceMsg || !msgInfo.From_Account || !msgInfo.MsgBody || msgInfo.MsgBody.length == 0) {
                                continue;
                            }

                            event = msgInfo.Event; //群消息类型
                            switch (event) {
                                case LONG_POLLINNG_EVENT_TYPE.GROUP_COMMON: //群普通消息
                                    log.info("bigGroupLongPolling: return new group msg");
                                    msg = handlerGroupMsg(msgInfo, false, false);
                                    msg && msgObjList.push(msg);
                                    msgCount = msgCount + 1;
                                    break;
                                case LONG_POLLINNG_EVENT_TYPE.GROUP_TIP: //群提示消息
                                case LONG_POLLINNG_EVENT_TYPE.GROUP_TIP2: //群提示消息
                                    log.info("bigGroupLongPolling: return new group tip");
                                    msg = handlerGroupMsg(msgInfo, false, false);
                                    msg && msgObjList.push(msg);
                                    //msgCount=msgCount+1;
                                    break;
                                case LONG_POLLINNG_EVENT_TYPE.GROUP_SYSTEM: //群系统消息
                                    log.info("bigGroupLongPolling: new group system msg");
                                    handlerGroupSystemMsg(msgInfo);
                                    break;
                                default:
                                    log.error("bigGroupLongPolling收到未知新消息类型: Event=" + event);
                                    break;
                            }
                        } // for loop
                        if (msgCount > 0) {
                            MsgManager.setBigGroupLongPollingMsgMap(msgInfo.ToGroupId, msgCount); //
                            log.warn("current bigGroupLongPollingMsgMap: " + JSON.stringify(bigGroupLongPollingMsgMap));
                        }
                    }
                    curBigGroupLongPollingRetErrorCount = 0;
                    //返回连接状态
                    var successInfo = {
                        'ActionStatus': ACTION_STATUS.OK,
                        'ErrorCode': CONNECTION_STATUS.ON,
                        'ErrorInfo': 'connection is ok...'
                    };
                    ConnManager.callBack(successInfo);

                    if (cbOk) cbOk(msgObjList);
                    else if (onBigGroupMsgCallback) onBigGroupMsgCallback(msgObjList); //返回新消息

                    //重新启动长轮询
                    bigGroupLongPollingOn && MsgManager.bigGroupLongPolling();

                }, function(err) {
                    //
                    if (err.ErrorCode != longPollingTimeOutErrorCode) {
                        log.error(err.ErrorInfo);
                        //记录长轮询返回错误次数
                        curBigGroupLongPollingRetErrorCount++;
                    }
                    if (err.ErrorCode == longPollingKickedErrorCode) {
                        //登出
                        log.error("多实例登录，被kick");
                        if (onKickedEventCall) {
                            onKickedEventCall();
                        }
                    }
                    //累计超过一定次数，不再发起长轮询请求
                    if (curBigGroupLongPollingRetErrorCount < LONG_POLLING_MAX_RET_ERROR_COUNT) {
                        bigGroupLongPollingOn && MsgManager.bigGroupLongPolling();
                    } else {
                        var errInfo = {
                            'ActionStatus': ACTION_STATUS.FAIL,
                            'ErrorCode': CONNECTION_STATUS.OFF,
                            'ErrorInfo': 'connection is off'
                        };
                        ConnManager.callBack(errInfo);
                    }
                    if (cbErr) cbErr(err);

                }, bigGroupLongPollingHoldTime * 1000);
            };

            //更新连接状态
            var updatecLongPollingStatus = function(errObj) {
                if (errObj.ErrorCode == 0 || errObj.ErrorCode == longPollingTimeOutErrorCode) {
                    curLongPollingRetErrorCount = 0;
                    longPollingOffCallbackFlag = false;
                    var errorInfo;
                    var isNeedCallback = false;
                    switch (curLongPollingStatus) {
                        case CONNECTION_STATUS.INIT:
                            isNeedCallback = true;
                            curLongPollingStatus = CONNECTION_STATUS.ON;
                            errorInfo = "create connection successfully(INIT->ON)";
                            break;
                        case CONNECTION_STATUS.ON:
                            errorInfo = "connection is on...(ON->ON)";
                            break;
                        case CONNECTION_STATUS.RECONNECT:
                            curLongPollingStatus = CONNECTION_STATUS.ON;
                            errorInfo = "connection is on...(RECONNECT->ON)";
                            break;
                        case CONNECTION_STATUS.OFF:
                            isNeedCallback = true;
                            curLongPollingStatus = CONNECTION_STATUS.RECONNECT;
                            errorInfo = "reconnect successfully(OFF->RECONNECT)";
                            break;
                    }
                    var successInfo = {
                        'ActionStatus': ACTION_STATUS.OK,
                        'ErrorCode': curLongPollingStatus,
                        'ErrorInfo': errorInfo
                    };
                    isNeedCallback && ConnManager.callBack(successInfo);
                    longPollingOn && MsgManager.longPolling();
                } else if (errObj.ErrorCode == longPollingKickedErrorCode) {
                    //登出
                    log.error("多实例登录，被kick");
                    if (onKickedEventCall) {
                        onKickedEventCall();
                    }
                } else {
                    //记录长轮询返回解析json错误次数
                    curLongPollingRetErrorCount++;
                    log.warn("longPolling接口第" + curLongPollingRetErrorCount + "次报错: " + errObj.ErrorInfo);
                    //累计超过一定次数
                    if (curLongPollingRetErrorCount <= LONG_POLLING_MAX_RET_ERROR_COUNT) {
                        setTimeout(startNextLongPolling, 100); //
                    } else {
                        curLongPollingStatus = CONNECTION_STATUS.OFF;
                        var errInfo = {
                            'ActionStatus': ACTION_STATUS.FAIL,
                            'ErrorCode': CONNECTION_STATUS.OFF,
                            'ErrorInfo': 'connection is off'
                        };
                        longPollingOffCallbackFlag == false && ConnManager.callBack(errInfo);
                        longPollingOffCallbackFlag = true;
                        log.warn(longPollingIntervalTime + "毫秒之后,SDK会发起新的longPolling请求...");
                        setTimeout(startNextLongPolling, longPollingIntervalTime); //长轮询接口报错次数达到一定值，每间隔5s发起新的长轮询
                    }
                }
            };

            //处理收到的普通C2C消息
            var handlerOrdinaryAndTipC2cMsgs = function(eventType, C2cMsgArray) {
                //处理c2c消息
                var notifyInfo = [];
                var msgInfos = [];
                msgInfos = C2cMsgArray; //返回的消息列表
                // MsgStore.cookie = resp.Cookie;//cookies，记录当前读到的最新消息位置

                for (var i in msgInfos) {
                    var msgInfo = msgInfos[i];
                    var isSendMsg, id, headUrl;
                    if (msgInfo.From_Account == ctx.identifier) { //当前用户发送的消息
                        isSendMsg = true;
                        id = msgInfo.To_Account; //读取接收者信息
                        headUrl = '';
                    } else { //当前用户收到的消息
                        isSendMsg = false;
                        id = msgInfo.From_Account; //读取发送者信息
                        headUrl = '';
                        var chatId = localStorage.getItem("cuToId");
                        if(id == chatId){
                        }else{
//                  		var data = "{\"dc\":\"msg001\"}";
//                      	plus.push.createMessage( "新消息", data, null );
                        }
                      
                    }
                    var sess = MsgStore.sessByTypeId(SESSION_TYPE.C2C, id);
                    if (!sess) {
                        sess = new Session(SESSION_TYPE.C2C, id, id, headUrl, 0, 0);
                    }
                    var msg = new Msg(sess, isSendMsg, msgInfo.MsgSeq, msgInfo.MsgRandom, msgInfo.MsgTimeStamp, msgInfo.From_Account);
                    var msgBody = null;
                    var msgContent = null;
                    var msgType = null;
                    for (var mi in msgInfo.MsgBody) {
                        msgBody = msgInfo.MsgBody[mi];
                        msgType = msgBody.MsgType;
                        switch (msgType) {
                            case MSG_ELEMENT_TYPE.TEXT:
                                msgContent = new Msg.Elem.Text(msgBody.MsgContent.Text);
                                break;
                            case MSG_ELEMENT_TYPE.FACE:
                                msgContent = new Msg.Elem.Face(
                                    msgBody.MsgContent.Index,
                                    msgBody.MsgContent.Data
                                );
                                break;
                            case MSG_ELEMENT_TYPE.IMAGE:
                                msgContent = new Msg.Elem.Images(
                                    msgBody.MsgContent.UUID,
                                    msgBody.MsgContent.ImageFormat || ""
                                );
                                for (var j in msgBody.MsgContent.ImageInfoArray) {
                                    var tempImg = msgBody.MsgContent.ImageInfoArray[j];
                                    msgContent.addImage(
                                        new Msg.Elem.Images.Image(
                                            tempImg.Type,
                                            tempImg.Size,
                                            tempImg.Width,
                                            tempImg.Height,
                                            tempImg.URL
                                        )
                                    );
                                }
                                break;
                            case MSG_ELEMENT_TYPE.SOUND:
                                if (msgBody.MsgContent) {
                                    msgContent = new Msg.Elem.Sound(
                                        msgBody.MsgContent.UUID,
                                        msgBody.MsgContent.Second,
                                        msgBody.MsgContent.Size,
                                        msgInfo.From_Account,
                                        msgInfo.To_Account,
                                        msgBody.MsgContent.Download_Flag,
                                        SESSION_TYPE.C2C
                                    );
                                } else {
                                    msgType = MSG_ELEMENT_TYPE.TEXT;
                                    msgContent = new Msg.Elem.Text('[语音消息]下载地址解析出错');
                                }
                                break;
                            case MSG_ELEMENT_TYPE.LOCATION:
                                msgContent = new Msg.Elem.Location(
                                    msgBody.MsgContent.Longitude,
                                    msgBody.MsgContent.Latitude,
                                    msgBody.MsgContent.Desc
                                );
                                break;
                            case MSG_ELEMENT_TYPE.FILE:
                            case MSG_ELEMENT_TYPE.FILE + " ":
                                msgType = MSG_ELEMENT_TYPE.FILE;
                                if (msgBody.MsgContent) {
                                    msgContent = new Msg.Elem.File(
                                        msgBody.MsgContent.UUID,
                                        msgBody.MsgContent.FileName,
                                        msgBody.MsgContent.FileSize,
                                        msgInfo.From_Account,
                                        msgInfo.To_Account,
                                        msgBody.MsgContent.Download_Flag,
                                        SESSION_TYPE.C2C
                                    );
                                } else {
                                    msgType = MSG_ELEMENT_TYPE.TEXT;
                                    msgContent = new Msg.Elem.Text('[文件消息下载地址解析出错]');
                                }
                                break;
                            case MSG_ELEMENT_TYPE.CUSTOM:
                                try {
                                    var data = JSON.parse(msgBody.MsgContent.Data);
                                    if (data && data.userAction && data.userAction == FRIEND_WRITE_MSG_ACTION.ING) { //过滤安卓或ios的正在输入自定义消息
                                        continue;
                                    }
                                } catch (e) {}

                                msgType = MSG_ELEMENT_TYPE.CUSTOM;
                                msgContent = new Msg.Elem.Custom(
                                    msgBody.MsgContent.Data,
                                    msgBody.MsgContent.Desc,
                                    msgBody.MsgContent.Ext
                                );
                                break;
                            default:
                                msgType = MSG_ELEMENT_TYPE.TEXT;
                                msgContent = new Msg.Elem.Text('web端暂不支持' + msgBody.MsgType + '消息');
                                break;
                        }
                        msg.elems.push(new Msg.Elem(msgType, msgContent));
                    }

                    if (msg.elems.length > 0 && MsgStore.addMsg(msg)) {
                        notifyInfo.push(msg);
                    }
                } // for loop
                if (notifyInfo.length > 0)
                    MsgStore.updateTimeline();
                if (notifyInfo.length > 0) {
                    if (onMsgCallback) onMsgCallback(notifyInfo);
                }
            };

            //发起新的长轮询请求
            var startNextLongPolling = function() {
                longPollingOn && MsgManager.longPolling();
            };

            //处理未决的加群申请消息列表
            var handlerApplyJoinGroupSystemMsgs = function(eventArray) {
                for (var i in eventArray) {
                    var e = eventArray[i];
                    handlerGroupSystemMsgs(e.GroupTips, true);
                    switch (e.Event) {
                        case LONG_POLLINNG_EVENT_TYPE.GROUP_SYSTEM: //（多终端同步）群系统消息
                            log.warn("handlerApplyJoinGroupSystemMsgs： handler new group system msg");
                            //true 表示 解决加群申请通知存在重复的问题（已处理的通知，下次登录还会拉到），需要判重
                            handlerGroupSystemMsgs(e.GroupTips, true);
                            break;
                        default:
                            log.error("syncMsgs收到未知的群系统消息类型: Event=" + e.Event);
                            break;
                    }
                }
            };

            //拉取c2c消息(包含加群未决消息，需要处理)
            this.syncMsgs = function(cbOk, cbErr) {
                var notifyInfo = [];
                var msgInfos = [];
                //读取C2C消息
                proto_getMsgs(MsgStore.cookie, MsgStore.syncFlag, function(resp) {
                    //拉取完毕
                    if (resp.SyncFlag == 2) {
                        MsgStore.syncFlag = 0;
                    }
                    //处理c2c消息
                    msgInfos = resp.MsgList; //返回的消息列表
                    MsgStore.cookie = resp.Cookie; //cookies，记录当前读到的最新消息位置

                    for (var i in msgInfos) {
                        var msgInfo = msgInfos[i];
                        var isSendMsg, id, headUrl;
                        if (msgInfo.From_Account == ctx.identifier) { //当前用户发送的消息
                            isSendMsg = true;
                            id = msgInfo.To_Account; //读取接收者信息
                            headUrl = '';
                        } else { //当前用户收到的消息
                            isSendMsg = false;
                            id = msgInfo.From_Account; //读取发送者信息
                            headUrl = '';
                        }
                        var sess = MsgStore.sessByTypeId(SESSION_TYPE.C2C, id);
                        if (!sess) {
                            sess = new Session(SESSION_TYPE.C2C, id, id, headUrl, 0, 0);
                        }
                        var msg = new Msg(sess, isSendMsg, msgInfo.MsgSeq, msgInfo.MsgRandom, msgInfo.MsgTimeStamp, msgInfo.From_Account);
                        var msgBody = null;
                        var msgContent = null;
                        var msgType = null;
                        for (var mi in msgInfo.MsgBody) {
                            msgBody = msgInfo.MsgBody[mi];
                            msgType = msgBody.MsgType;
                            switch (msgType) {
                                case MSG_ELEMENT_TYPE.TEXT:
                                    msgContent = new Msg.Elem.Text(msgBody.MsgContent.Text);
                                    break;
                                case MSG_ELEMENT_TYPE.FACE:
                                    msgContent = new Msg.Elem.Face(
                                        msgBody.MsgContent.Index,
                                        msgBody.MsgContent.Data
                                    );
                                    break;
                                case MSG_ELEMENT_TYPE.IMAGE:
                                    msgContent = new Msg.Elem.Images(
                                        msgBody.MsgContent.UUID,
                                        msgBody.MsgContent.ImageFormat
                                    );
                                    for (var j in msgBody.MsgContent.ImageInfoArray) {
                                        var tempImg = msgBody.MsgContent.ImageInfoArray[j];
                                        msgContent.addImage(
                                            new Msg.Elem.Images.Image(
                                                tempImg.Type,
                                                tempImg.Size,
                                                tempImg.Width,
                                                tempImg.Height,
                                                tempImg.URL
                                            )
                                        );
                                    }
                                    break;
                                case MSG_ELEMENT_TYPE.SOUND:
                                    // var soundUrl = getSoundDownUrl(msgBody.MsgContent.UUID, msgInfo.From_Account);
                                    if (msgBody.MsgContent) {
                                        msgContent = new Msg.Elem.Sound(
                                            msgBody.MsgContent.UUID,
                                            msgBody.MsgContent.Second,
                                            msgBody.MsgContent.Size,
                                            msgInfo.From_Account,
                                            msgInfo.To_Account,
                                            msgBody.MsgContent.Download_Flag,
                                            SESSION_TYPE.C2C
                                        );
                                    } else {
                                        msgType = MSG_ELEMENT_TYPE.TEXT;
                                        msgContent = new Msg.Elem.Text('[语音消息]下载地址解析出错');
                                    }
                                    break;
                                case MSG_ELEMENT_TYPE.LOCATION:
                                    msgContent = new Msg.Elem.Location(
                                        msgBody.MsgContent.Longitude,
                                        msgBody.MsgContent.Latitude,
                                        msgBody.MsgContent.Desc
                                    );
                                    break;
                                case MSG_ELEMENT_TYPE.FILE:
                                case MSG_ELEMENT_TYPE.FILE + " ":
                                    msgType = MSG_ELEMENT_TYPE.FILE;
                                    // var fileUrl = getFileDownUrl(msgBody.MsgContent.UUID, msgInfo.From_Account, msgBody.MsgContent.FileName);
                                    if (msgBody.MsgContent) {
                                        msgContent = new Msg.Elem.File(
                                            msgBody.MsgContent.UUID,
                                            msgBody.MsgContent.FileName,
                                            msgBody.MsgContent.FileSize,
                                            msgInfo.From_Account,
                                            msgInfo.To_Account,
                                            msgBody.MsgContent.Download_Flag,
                                            SESSION_TYPE.C2C
                                        );
                                    } else {
                                        msgType = MSG_ELEMENT_TYPE.TEXT;
                                        msgContent = new Msg.Elem.Text('[文件消息下载地址解析出错]');
                                    }
                                    break;
                                case MSG_ELEMENT_TYPE.CUSTOM:
                                    try {
                                        var data = JSON.parse(msgBody.MsgContent.Data);
                                        if (data && data.userAction && data.userAction == FRIEND_WRITE_MSG_ACTION.ING) { //过滤安卓或ios的正在输入自定义消息
                                            continue;
                                        }
                                    } catch (e) {}

                                    msgType = MSG_ELEMENT_TYPE.CUSTOM;
                                    msgContent = new Msg.Elem.Custom(
                                        msgBody.MsgContent.Data,
                                        msgBody.MsgContent.Desc,
                                        msgBody.MsgContent.Ext
                                    );
                                    break;
                                default:
                                    msgType = MSG_ELEMENT_TYPE.TEXT;
                                    msgContent = new Msg.Elem.Text('web端暂不支持' + msgBody.MsgType + '消息');
                                    break;
                            }
                            msg.elems.push(new Msg.Elem(msgType, msgContent));
                        }

                        if (msg.elems.length > 0 && MsgStore.addMsg(msg)) {
                            notifyInfo.push(msg);
                        }
                    } // for loop

                    //处理加群未决申请消息
                    handlerApplyJoinGroupSystemMsgs(resp.EventArray);

                    if (notifyInfo.length > 0)
                        MsgStore.updateTimeline();
                    if (cbOk) cbOk(notifyInfo);
                    else if (notifyInfo.length > 0) {
                        if (onMsgCallback) onMsgCallback(notifyInfo);
                    }

                }, function(err) {
                    log.error("getMsgs failed:" + err.ErrorInfo);
                    if (cbErr) cbErr(err);
                });
            };


            //拉取C2C漫游消息
            this.getC2CHistoryMsgs = function(options, cbOk, cbErr) {

                if (!options.Peer_Account) {
                    if (cbErr) {
                        cbErr(tool.getReturnError("Peer_Account is empty", -13));
                        return;
                    }
                }
                if (!options.MaxCnt) {
                    options.MaxCnt = 15;
                }
                if (options.MaxCnt <= 0) {
                    if (cbErr) {
                        cbErr(tool.getReturnError("MaxCnt should be greater than 0", -14));
                        return;
                    }
                }
                if (options.MaxCnt > 15) {
                    if (cbErr) {
                        cbErr(tool.getReturnError("MaxCnt can not be greater than 15", -15));
                        return;
                    }
                    return;
                }
                if (options.MsgKey == null || options.MsgKey === undefined) {
                    options.MsgKey = '';
                }
                var opts = {
                    'Peer_Account': options.Peer_Account,
                    'MaxCnt': options.MaxCnt,
                    'LastMsgTime': options.LastMsgTime,
                    'MsgKey': options.MsgKey
                };
                //读取c2c漫游消息
                proto_getC2CHistoryMsgs(opts, function(resp) {
                    var msgObjList = [];
                    var msgInfos = [];
                    //处理c2c消息
                    msgInfos = resp.MsgList; //返回的消息列表
                    var sess = MsgStore.sessByTypeId(SESSION_TYPE.C2C, options.Peer_Account);
                    if (!sess) {
                        sess = new Session(SESSION_TYPE.C2C, options.Peer_Account, options.Peer_Account, '', 0, 0);
                    }
                    for (var i in msgInfos) {
                        var msgInfo = msgInfos[i];
                        var isSendMsg, id, headUrl;
                        if (msgInfo.From_Account == ctx.identifier) { //当前用户发送的消息
                            isSendMsg = true;
                            id = msgInfo.To_Account; //读取接收者信息
                            headUrl = '';
                        } else { //当前用户收到的消息
                            isSendMsg = false;
                            id = msgInfo.From_Account; //读取发送者信息
                            headUrl = '';
                        }
                        var msg = new Msg(sess, isSendMsg, msgInfo.MsgSeq, msgInfo.MsgRandom, msgInfo.MsgTimeStamp, msgInfo.From_Account);
                        var msgBody = null;
                        var msgContent = null;
                        var msgType = null;
                        for (var mi in msgInfo.MsgBody) {
                            msgBody = msgInfo.MsgBody[mi];
                            msgType = msgBody.MsgType;
                            switch (msgType) {
                                case MSG_ELEMENT_TYPE.TEXT:
                                    msgContent = new Msg.Elem.Text(msgBody.MsgContent.Text);
                                    break;
                                case MSG_ELEMENT_TYPE.FACE:
                                    msgContent = new Msg.Elem.Face(
                                        msgBody.MsgContent.Index,
                                        msgBody.MsgContent.Data
                                    );
                                    break;
                                case MSG_ELEMENT_TYPE.IMAGE:
                                    msgContent = new Msg.Elem.Images(
                                        msgBody.MsgContent.UUID,
                                        msgBody.MsgContent.ImageFormat
                                    );
                                    for (var j in msgBody.MsgContent.ImageInfoArray) {
                                        var tempImg = msgBody.MsgContent.ImageInfoArray[j];
                                        msgContent.addImage(
                                            new Msg.Elem.Images.Image(
                                                tempImg.Type,
                                                tempImg.Size,
                                                tempImg.Width,
                                                tempImg.Height,
                                                tempImg.URL
                                            )
                                        );
                                    }
                                    break;
                                case MSG_ELEMENT_TYPE.SOUND:

                                    // var soundUrl = getSoundDownUrl(msgBody.MsgContent.UUID, msgInfo.From_Account);

                                    if (msgBody.MsgContent) {
                                        msgContent = new Msg.Elem.Sound(
                                            msgBody.MsgContent.UUID,
                                            msgBody.MsgContent.Second,
                                            msgBody.MsgContent.Size,
                                            msgInfo.From_Account,
                                            msgInfo.To_Account,
                                            msgBody.MsgContent.Download_Flag,
                                            SESSION_TYPE.C2C
                                        );
                                    } else {
                                        msgType = MSG_ELEMENT_TYPE.TEXT;
                                        msgContent = new Msg.Elem.Text('[语音消息]下载地址解析出错');
                                    }
                                    break;
                                case MSG_ELEMENT_TYPE.LOCATION:
                                    msgContent = new Msg.Elem.Location(
                                        msgBody.MsgContent.Longitude,
                                        msgBody.MsgContent.Latitude,
                                        msgBody.MsgContent.Desc
                                    );
                                    break;
                                case MSG_ELEMENT_TYPE.FILE:
                                case MSG_ELEMENT_TYPE.FILE + " ":
                                    msgType = MSG_ELEMENT_TYPE.FILE;
                                    // var fileUrl = getFileDownUrl(msgBody.MsgContent.UUID, msgInfo.From_Account, msgBody.MsgContent.FileName);

                                    if (msgBody.MsgContent) {
                                        msgContent = new Msg.Elem.File(
                                            msgBody.MsgContent.UUID,
                                            msgBody.MsgContent.FileName,
                                            msgBody.MsgContent.FileSize,
                                            msgInfo.From_Account,
                                            msgInfo.To_Account,
                                            msgBody.MsgContent.Download_Flag,
                                            SESSION_TYPE.C2C
                                        );
                                    } else {
                                        msgType = MSG_ELEMENT_TYPE.TEXT;
                                        msgContent = new Msg.Elem.Text('[文件消息下载地址解析出错]');
                                    }
                                    break;
                                case MSG_ELEMENT_TYPE.CUSTOM:
                                    msgType = MSG_ELEMENT_TYPE.CUSTOM;
                                    msgContent = new Msg.Elem.Custom(
                                        msgBody.MsgContent.Data,
                                        msgBody.MsgContent.Desc,
                                        msgBody.MsgContent.Ext
                                    );

                                    break;
                                default:
                                    msgType = MSG_ELEMENT_TYPE.TEXT;
                                    msgContent = new Msg.Elem.Text('web端暂不支持' + msgBody.MsgType + '消息');
                                    break;
                            }
                            msg.elems.push(new Msg.Elem(msgType, msgContent));
                        }
                        MsgStore.addMsg(msg);
                        msgObjList.push(msg);
                    } // for loop

                    MsgStore.updateTimeline();
                    if (cbOk) {

                        var newResp = {
                            'Complete': resp.Complete,
                            'MsgCount': msgObjList.length,
                            'LastMsgTime': resp.LastMsgTime,
                            'MsgKey': resp.MsgKey,
                            'MsgList': msgObjList
                        };
                        sess.isFinished(resp.Complete);
                        cbOk(newResp);
                    }

                }, function(err) {
                    log.error("getC2CHistoryMsgs failed:" + err.ErrorInfo);
                    if (cbErr) cbErr(err);
                });
            };

            //拉群历史消息
            //不传cbOk 和 cbErr，则会调用新消息回调函数
            this.syncGroupMsgs = function(options, cbOk, cbErr) {
                if (options.ReqMsgSeq <= 0) {
                    if (cbErr) {
                        var errInfo = "ReqMsgSeq must be greater than 0";
                        var error = tool.getReturnError(errInfo, -16);
                        cbErr(error);
                    }
                    return;
                }
                var opts = {
                    'GroupId': options.GroupId,
                    'ReqMsgSeq': options.ReqMsgSeq,
                    'ReqMsgNumber': options.ReqMsgNumber
                };
                //读群漫游消息
                proto_getGroupMsgs(opts, function(resp) {
                    var notifyInfo = [];
                    var group_id = resp.GroupId; //返回的群id
                    var msgInfos = resp.RspMsgList; //返回的消息列表
                    var isFinished = resp.IsFinished;

                    if (msgInfos == null || msgInfos === undefined) {
                        if (cbOk) {
                            cbOk([]);
                        }
                        return;
                    }
                    for (var i = msgInfos.length - 1; i >= 0; i--) {
                        var msgInfo = msgInfos[i];
                        //如果是已经删除的消息或者发送者帐号为空或者消息内容为空
                        //IsPlaceMsg=1
                        if (msgInfo.IsPlaceMsg || !msgInfo.From_Account || !msgInfo.MsgBody || msgInfo.MsgBody.length == 0) {
                            continue;
                        }
                        var msg = handlerGroupMsg(msgInfo, true, true, isFinished);
                        if (msg) {
                            notifyInfo.push(msg);
                        }
                    } // for loop
                    if (notifyInfo.length > 0)
                        MsgStore.updateTimeline();
                    if (cbOk) cbOk(notifyInfo);
                    else if (notifyInfo.length > 0) {
                        if (onMsgCallback) onMsgCallback(notifyInfo);
                    }

                }, function(err) {
                    log.error("getGroupMsgs failed:" + err.ErrorInfo);
                    if (cbErr) cbErr(err);
                });
            };

            //处理群消息(普通消息+提示消息)
            //isSyncGroupMsgs 是否主动拉取群消息标志
            //isAddMsgFlag 是否需要保存到MsgStore，如果需要，这里会存在判重逻辑
            var handlerGroupMsg = function(msgInfo, isSyncGroupMsgs, isAddMsgFlag, isFinished) {
                if (msgInfo.IsPlaceMsg || !msgInfo.From_Account || !msgInfo.MsgBody || msgInfo.MsgBody.length == 0) {
                    return null;
                }
                var isSendMsg, id, headUrl, fromAccountNick, fromAccountHeadurl;
                var group_id = msgInfo.ToGroupId;
                var group_name = group_id;
                if (msgInfo.GroupInfo) { //取出群名称
                    if (msgInfo.GroupInfo.GroupName) {
                        group_name = msgInfo.GroupInfo.GroupName;
                    }
                }
                //取出成员昵称
                fromAccountNick = msgInfo.From_Account;
                //fromAccountHeadurl = msgInfo.GroupInfo.From_AccountHeadurl;
                if (msgInfo.GroupInfo) {
                    if (msgInfo.GroupInfo.From_AccountNick) {
                        fromAccountNick = msgInfo.GroupInfo.From_AccountNick;

                    }
                    if (msgInfo.GroupInfo.From_AccountHeadurl) {
                        fromAccountHeadurl = msgInfo.GroupInfo.From_AccountHeadurl;
                    } else {
                        fromAccountHeadurl = null;
                    }
                }
                if (msgInfo.From_Account == ctx.identifier) { //当前用户发送的消息
                    isSendMsg = true;
                    id = msgInfo.From_Account; //读取接收者信息
                    headUrl = '';
                } else { //当前用户收到的消息
                    isSendMsg = false;
                    id = msgInfo.From_Account; //读取发送者信息
                    headUrl = '';
                }
                var sess = MsgStore.sessByTypeId(SESSION_TYPE.GROUP, group_id);
                if (!sess) {
                    sess = new Session(SESSION_TYPE.GROUP, group_id, group_name, headUrl, 0, 0);
                }
                if (typeof isFinished !== "undefined") {
                    sess.isFinished(isFinished || 0);
                }
                var subType = GROUP_MSG_SUB_TYPE.COMMON; //消息类型
                //群提示消息,重新封装下
                if (LONG_POLLINNG_EVENT_TYPE.GROUP_TIP == msgInfo.Event || LONG_POLLINNG_EVENT_TYPE.GROUP_TIP2 == msgInfo.Event) {
                    subType = GROUP_MSG_SUB_TYPE.TIP;
                    var groupTip = msgInfo.MsgBody;
                    msgInfo.MsgBody = [];
                    msgInfo.MsgBody.push({
                        "MsgType": MSG_ELEMENT_TYPE.GROUP_TIP,
                        "MsgContent": groupTip
                    });
                } else if (msgInfo.MsgPriority) { //群点赞消息
                    if (msgInfo.MsgPriority == GROUP_MSG_PRIORITY_TYPE.REDPACKET) {
                        subType = GROUP_MSG_SUB_TYPE.REDPACKET;
                    } else if (msgInfo.MsgPriority == GROUP_MSG_PRIORITY_TYPE.LOVEMSG) {
                        subType = GROUP_MSG_SUB_TYPE.LOVEMSG;
                    }

                }
                var msg = new Msg(sess, isSendMsg, msgInfo.MsgSeq, msgInfo.MsgRandom, msgInfo.MsgTimeStamp, msgInfo.From_Account, subType, fromAccountNick, fromAccountHeadurl);
                var msgBody = null;
                var msgContent = null;
                var msgType = null;
                for (var mi in msgInfo.MsgBody) {
                    msgBody = msgInfo.MsgBody[mi];
                    msgType = msgBody.MsgType;
                    switch (msgType) {
                        case MSG_ELEMENT_TYPE.TEXT:
                            msgContent = new Msg.Elem.Text(msgBody.MsgContent.Text);
                            break;
                        case MSG_ELEMENT_TYPE.FACE:
                            msgContent = new Msg.Elem.Face(
                                msgBody.MsgContent.Index,
                                msgBody.MsgContent.Data
                            );
                            break;
                        case MSG_ELEMENT_TYPE.IMAGE:
                            msgContent = new Msg.Elem.Images(
                                msgBody.MsgContent.UUID,
                                msgBody.MsgContent.ImageFormat || ""
                            );
                            for (var j in msgBody.MsgContent.ImageInfoArray) {
                                msgContent.addImage(
                                    new Msg.Elem.Images.Image(
                                        msgBody.MsgContent.ImageInfoArray[j].Type,
                                        msgBody.MsgContent.ImageInfoArray[j].Size,
                                        msgBody.MsgContent.ImageInfoArray[j].Width,
                                        msgBody.MsgContent.ImageInfoArray[j].Height,
                                        msgBody.MsgContent.ImageInfoArray[j].URL
                                    )
                                );
                            }
                            break;
                        case MSG_ELEMENT_TYPE.SOUND:
                            if (msgBody.MsgContent) {
                                msgContent = new Msg.Elem.Sound(
                                    msgBody.MsgContent.UUID,
                                    msgBody.MsgContent.Second,
                                    msgBody.MsgContent.Size,
                                    msgInfo.From_Account,
                                    msgInfo.To_Account,
                                    msgBody.MsgContent.Download_Flag,
                                    SESSION_TYPE.GROUP
                                );
                            } else {
                                msgType = MSG_ELEMENT_TYPE.TEXT;
                                msgContent = new Msg.Elem.Text('[语音消息]下载地址解析出错');
                            }
                            break;
                        case MSG_ELEMENT_TYPE.LOCATION:
                            msgContent = new Msg.Elem.Location(
                                msgBody.MsgContent.Longitude,
                                msgBody.MsgContent.Latitude,
                                msgBody.MsgContent.Desc
                            );
                            break;
                        case MSG_ELEMENT_TYPE.FILE:
                        case MSG_ELEMENT_TYPE.FILE + " ":
                            msgType = MSG_ELEMENT_TYPE.FILE;
                            var fileUrl = getFileDownUrl(msgBody.MsgContent.UUID, msgInfo.From_Account, msgBody.MsgContent.FileName);

                            if (msgBody.MsgContent) {
                                msgContent = new Msg.Elem.File(
                                    msgBody.MsgContent.UUID,
                                    msgBody.MsgContent.FileName,
                                    msgBody.MsgContent.FileSize,
                                    msgInfo.From_Account,
                                    msgInfo.To_Account,
                                    msgBody.MsgContent.Download_Flag,
                                    SESSION_TYPE.GROUP
                                );
                            } else {
                                msgType = MSG_ELEMENT_TYPE.TEXT;
                                msgContent = new Msg.Elem.Text('[文件消息]地址解析出错');
                            }
                            break;
                        case MSG_ELEMENT_TYPE.GROUP_TIP:
                            var opType = msgBody.MsgContent.OpType;
                            msgContent = new Msg.Elem.GroupTip(
                                opType,
                                msgBody.MsgContent.Operator_Account,
                                group_id,
                                msgInfo.GroupInfo.GroupName,
                                msgBody.MsgContent.List_Account
                            );
                            if (GROUP_TIP_TYPE.JOIN == opType || GROUP_TIP_TYPE.QUIT == opType) { //加群或退群时，设置最新群成员数
                                msgContent.setGroupMemberNum(msgBody.MsgContent.MemberNum);
                            } else if (GROUP_TIP_TYPE.MODIFY_GROUP_INFO == opType) { //群资料变更
                                var tempIsCallbackFlag = false;
                                var tempNewGroupInfo = {
                                    "GroupId": group_id,
                                    "GroupFaceUrl": null,
                                    "GroupName": null,
                                    "OwnerAccount": null,
                                    "GroupNotification": null,
                                    "GroupIntroduction": null
                                };
                                var msgGroupNewInfo = msgBody.MsgContent.MsgGroupNewInfo;
                                if (msgGroupNewInfo.GroupFaceUrl) {
                                    var tmpNGIFaceUrl = new Msg.Elem.GroupTip.GroupInfo(
                                        GROUP_TIP_MODIFY_GROUP_INFO_TYPE.FACE_URL,
                                        msgGroupNewInfo.GroupFaceUrl
                                    );
                                    msgContent.addGroupInfo(tmpNGIFaceUrl);
                                    tempIsCallbackFlag = true;
                                    tempNewGroupInfo.GroupFaceUrl = msgGroupNewInfo.GroupFaceUrl;
                                }
                                if (msgGroupNewInfo.GroupName) {
                                    var tmpNGIName = new Msg.Elem.GroupTip.GroupInfo(
                                        GROUP_TIP_MODIFY_GROUP_INFO_TYPE.NAME,
                                        msgGroupNewInfo.GroupName
                                    );
                                    msgContent.addGroupInfo(tmpNGIName);
                                    tempIsCallbackFlag = true;
                                    tempNewGroupInfo.GroupName = msgGroupNewInfo.GroupName;
                                }
                                if (msgGroupNewInfo.Owner_Account) {
                                    var tmpNGIOwner = new Msg.Elem.GroupTip.GroupInfo(
                                        GROUP_TIP_MODIFY_GROUP_INFO_TYPE.OWNER,
                                        msgGroupNewInfo.Owner_Account
                                    );
                                    msgContent.addGroupInfo(tmpNGIOwner);
                                    tempIsCallbackFlag = true;
                                    tempNewGroupInfo.OwnerAccount = msgGroupNewInfo.Owner_Account;
                                }
                                if (msgGroupNewInfo.GroupNotification) {
                                    var tmpNGINotification = new Msg.Elem.GroupTip.GroupInfo(
                                        GROUP_TIP_MODIFY_GROUP_INFO_TYPE.NOTIFICATION,
                                        msgGroupNewInfo.GroupNotification
                                    );
                                    msgContent.addGroupInfo(tmpNGINotification);
                                    tempIsCallbackFlag = true;
                                    tempNewGroupInfo.GroupNotification = msgGroupNewInfo.GroupNotification;
                                }
                                if (msgGroupNewInfo.GroupIntroduction) {
                                    var tmpNGIIntroduction = new Msg.Elem.GroupTip.GroupInfo(
                                        GROUP_TIP_MODIFY_GROUP_INFO_TYPE.INTRODUCTION,
                                        msgGroupNewInfo.GroupIntroduction
                                    );
                                    msgContent.addGroupInfo(tmpNGIIntroduction);
                                    tempIsCallbackFlag = true;
                                    tempNewGroupInfo.GroupIntroduction = msgGroupNewInfo.GroupIntroduction;
                                }

                                //回调群资料变化通知方法
                                if (isSyncGroupMsgs == false && tempIsCallbackFlag && onGroupInfoChangeCallback) {
                                    onGroupInfoChangeCallback(tempNewGroupInfo);
                                }

                            } else if (GROUP_TIP_TYPE.MODIFY_MEMBER_INFO == opType) { //群成员变更
                                var memberInfos = msgBody.MsgContent.MsgMemberInfo;
                                for (var n in memberInfos) {
                                    var memberInfo = memberInfos[n];
                                    msgContent.addMemberInfo(
                                        new Msg.Elem.GroupTip.MemberInfo(
                                            memberInfo.User_Account, memberInfo.ShutupTime
                                        )
                                    );
                                }
                            }
                            break;
                        case MSG_ELEMENT_TYPE.CUSTOM:
                            msgType = MSG_ELEMENT_TYPE.CUSTOM;
                            msgContent = new Msg.Elem.Custom(
                                msgBody.MsgContent.Data,
                                msgBody.MsgContent.Desc,
                                msgBody.MsgContent.Ext
                            );
                            break;
                        default:
                            msgType = MSG_ELEMENT_TYPE.TEXT;
                            msgContent = new Msg.Elem.Text('web端暂不支持' + msgBody.MsgType + '消息');
                            break;
                    }
                    msg.elems.push(new Msg.Elem(msgType, msgContent));
                }

                if (isAddMsgFlag == false) { //不需要保存消息
                    return msg;
                }

                if (MsgStore.addMsg(msg)) {
                    return msg;
                } else {
                    return null;
                }
            };

            //初始化
            this.init = function(listeners, cbOk, cbErr) {
                if (!listeners.onMsgNotify) {
                    log.warn('listeners.onMsgNotify is empty');
                }
                onMsgCallback = listeners.onMsgNotify;

                if (listeners.onBigGroupMsgNotify) {
                    onBigGroupMsgCallback = listeners.onBigGroupMsgNotify;
                } else {
                    log.warn('listeners.onBigGroupMsgNotify is empty');
                }

                if (listeners.onC2cEventNotifys) {
                    onC2cEventCallbacks = listeners.onC2cEventNotifys;
                } else {
                    log.warn('listeners.onC2cEventNotifys is empty');
                }
                if (listeners.onGroupSystemNotifys) {
                    onGroupSystemNotifyCallbacks = listeners.onGroupSystemNotifys;
                } else {
                    log.warn('listeners.onGroupSystemNotifys is empty');
                }
                if (listeners.onGroupInfoChangeNotify) {
                    onGroupInfoChangeCallback = listeners.onGroupInfoChangeNotify;
                } else {
                    log.warn('listeners.onGroupInfoChangeNotify is empty');
                }
                if (listeners.onFriendSystemNotifys) {
                    onFriendSystemNotifyCallbacks = listeners.onFriendSystemNotifys;
                } else {
                    log.warn('listeners.onFriendSystemNotifys is empty');
                }
                if (listeners.onProfileSystemNotifys) {
                    onProfileSystemNotifyCallbacks = listeners.onProfileSystemNotifys;
                } else {
                    log.warn('listeners.onProfileSystemNotifys is empty');
                }
                if (listeners.onKickedEventCall) {
                    onKickedEventCall = listeners.onKickedEventCall;
                } else {
                    log.warn('listeners.onKickedEventCall is empty');
                }

                if (listeners.onAppliedDownloadUrl) {
                    onAppliedDownloadUrl = listeners.onAppliedDownloadUrl;
                } else {
                    log.warn('listeners.onAppliedDownloadUrl is empty');
                }

                if (!ctx.identifier || !ctx.userSig) {
                    if (cbOk) {
                        var success = {
                            'ActionStatus': ACTION_STATUS.OK,
                            'ErrorCode': 0,
                            'ErrorInfo': "login success(no login state)"
                        };
                        cbOk(success);
                    }
                    return;
                }

                //初始化
                initMyGroupMaxSeqs(
                    function(resp) {
                        log.info('initMyGroupMaxSeqs success');
                        //初始化文件
                        initIpAndAuthkey(
                            function(initIpAndAuthkeyResp) {
                                log.info('initIpAndAuthkey success');
                                if (cbOk) {
                                    log.info('login success(have login state))');
                                    var success = {
                                        'ActionStatus': ACTION_STATUS.OK,
                                        'ErrorCode': 0,
                                        'ErrorInfo': "login success"
                                    };
                                    cbOk(success);
                                }
                                MsgManager.setLongPollingOn(true); //开启长轮询
                                longPollingOn && MsgManager.longPolling(cbOk);
                            }, cbErr);
                    }, cbErr);
            };

            //发消息（私聊或群聊）
            this.sendMsg = function(msg, cbOk, cbErr) {
                proto_sendMsg(msg, function(resp) {
                    //私聊时，加入自己的发的消息，群聊时，由于seq和服务器的seq不一样，所以不作处理
                    if (msg.sess.type() == SESSION_TYPE.C2C) {
                        if (!MsgStore.addMsg(msg)) {
                            var errInfo = "sendMsg: addMsg failed!";
                            var error = tool.getReturnError(errInfo, -17);
                            log.error(errInfo);
                            if (cbErr) cbErr(error);
                            return;
                        }
                        //更新信息流时间
                        MsgStore.updateTimeline();
                    }
                    if (cbOk) cbOk(resp);
                }, function(err) {
                    if (cbErr) cbErr(err);
                });
            };
        };

    //上传文件
    var FileUploader = new function() {
            this.fileMd5 = null;
            //获取文件MD5
            var getFileMD5 = function(file, cbOk, cbErr) {

                //FileReader pc浏览器兼容性
                //Feature   Firefox (Gecko) Chrome  Internet Explorer   Opera   Safari
                //Basic support 3.6 7   10                      12.02   6.0.2
                var fileReader = null;
                try {
                    fileReader = new FileReader(); //分块读取文件对象
                } catch (e) {
                    if (cbErr) {
                        cbErr(tool.getReturnError('当前浏览器不支持FileReader', -18));
                        return;
                    }
                }
                //file的slice方法，注意它的兼容性，在不同浏览器的写法不同
                var blobSlice = File.prototype.mozSlice || File.prototype.webkitSlice || File.prototype.slice;
                if (!blobSlice) {
                    if (cbErr) {
                        cbErr(tool.getReturnError('当前浏览器不支持FileAPI', -19));
                        return;
                    }
                }

                var chunkSize = 2 * 1024 * 1024; //分块大小，2M
                var chunks = Math.ceil(file.size / chunkSize); //总块数
                var currentChunk = 0; //当前块数
                var spark = new SparkMD5(); //获取MD5对象

                fileReader.onload = function(e) { //数据加载完毕事件

                    var binaryStr = "";
                    var bytes = new Uint8Array(e.target.result);
                    var length = bytes.byteLength;
                    for (var i = 0; i < length; i++) {
                        binaryStr += String.fromCharCode(bytes[i]); //二进制转换字符串
                    }
                    spark.appendBinary(binaryStr);
                    currentChunk++;
                    if (currentChunk < chunks) {
                        loadNext(); //读取下一块数据
                    } else {
                        this.fileMd5 = spark.end(); //得到文件MD5值
                        if (cbOk) {
                            cbOk(this.fileMd5);
                        }
                    }
                };
                //分片读取文件

                function loadNext() {
                    var start = currentChunk * chunkSize,
                        end = start + chunkSize >= file.size ? file.size : start + chunkSize;
                    //根据开始和结束位置，切割文件
                    var b = blobSlice.call(file, start, end);
                    //readAsBinaryString ie浏览器不兼容此方法
                    //fileReader.readAsBinaryString(blobSlice.call(file, start, end));
                    fileReader.readAsArrayBuffer(b); //ie，chrome，firefox等主流浏览器兼容此方法

                }

                loadNext(); //开始读取
            };
            //提交上传图片表单(用于低版本IE9以下)
            this.submitUploadFileForm = function(options, cbOk, cbErr) {
                var errInfo;
                var error;
                var formId = options.formId;
                var fileId = options.fileId;
                var iframeNum = uploadResultIframeId++;
                var iframeName = "uploadResultIframe_" + iframeNum;
                var toAccount = options.To_Account;
                var businessType = options.businessType;

                var form = document.getElementById(formId);
                if (!form) {
                    errInfo = "获取表单对象为空: formId=" + formId + "(formId非法)";
                    error = tool.getReturnError(errInfo, -20);
                    if (cbErr) cbErr(error);
                    return;
                }

                var fileObj = document.getElementById(fileId);
                if (!fileObj) {
                    errInfo = "获取文件对象为空: fileId=" + fileId + "(没有选择文件或者fileId非法)";
                    error = tool.getReturnError(errInfo, -21);
                    if (cbErr) cbErr(error);
                    return;
                }
                //fileObj.type="file";//ie8下不起作用，必须由业务自己设置
                fileObj.name = "file";

                var iframe = document.createElement("iframe");
                iframe.name = iframeName;
                iframe.id = iframeName;
                iframe.style.display = "none";
                document.body.appendChild(iframe);

                var cmdName;
                if (isAccessFormalEnv()) {
                    cmdName = 'pic_up';
                } else {
                    cmdName = 'pic_up_test';
                }
                var uploadApiUrl = "https://pic.tim.qq.com/v4/openpic/" + cmdName + "?tinyid=" + ctx.tinyid + "&a2=" + ctx.a2 + "&sdkappid=" + ctx.sdkAppID + "&contenttype=http" + "&accounttype=" + ctx.accountType;
                form.action = uploadApiUrl;
                form.method = 'post';
                //form.enctype='multipart/form-data';//ie8下不起作用，必须由业务自己设置
                form.target = iframeName;

                function createFormInput(name, value) {
                    var tempInput = document.createElement("input");
                    tempInput.type = "hidden";
                    tempInput.name = name;
                    tempInput.value = value;
                    form.appendChild(tempInput);
                }

                createFormInput("App_Version", VERSION_INFO.APP_VERSION);
                createFormInput("From_Account", ctx.identifier);
                createFormInput("To_Account", toAccount);
                createFormInput("Seq", nextSeq().toString());
                createFormInput("Timestamp", unixtime().toString());
                createFormInput("Random", createRandom().toString());
                createFormInput("Busi_Id", businessType);
                createFormInput("PkgFlag", UPLOAD_RES_PKG_FLAG.RAW_DATA.toString());
                createFormInput("Auth_Key", authkey);
                createFormInput("Server_Ver", VERSION_INFO.SERVER_VERSION.toString());
                createFormInput("File_Type", options.fileType);


                //检测iframe.contentWindow.name是否有值

                function checkFrameName() {
                    var resp;
                    try {
                        resp = JSON.parse(iframe.contentWindow.name) || {};
                    } catch (e) {
                        resp = {};
                    }
                    if (resp.ActionStatus) { //上传接口返回
                        // We've got what we need. Stop the iframe from loading further content.
                        iframe.src = "about:blank";
                        iframe.parentNode.removeChild(iframe);
                        iframe = null;

                        if (resp.ActionStatus == ACTION_STATUS.OK) {
                            cbOk && cbOk(resp);
                        } else {
                            cbErr && cbErr(resp);
                        }
                    } else {
                        setTimeout(checkFrameName, 100);
                    }
                }

                setTimeout(checkFrameName, 500);

                form.submit(); //提交上传图片表单
            };
            //上传图片或文件(用于高版本浏览器，支持FileAPI)
            this.uploadFile = function(options, cbOk, cbErr) {

                var file_upload = {
                    //初始化
                    init: function(options, cbOk, cbErr) {
                        var me = this;
                        me.file = options.file;
                        //分片上传进度回调事件
                        me.onProgressCallBack = options.onProgressCallBack;
                        //停止上传图片按钮
                        if (options.abortButton) {
                            options.abortButton.onclick = me.abortHandler;
                        }
                        me.total = me.file.size; //文件总大小
                        me.loaded = 0; //已读取字节数
                        me.step = 1080 * 1024; //分块大小，1080K
                        me.sliceSize = 0; //分片大小
                        me.sliceOffset = 0; //当前分片位置
                        me.timestamp = unixtime(); //当前时间戳
                        me.seq = nextSeq(); //请求seq
                        me.random = createRandom(); //请求随机数
                        me.fromAccount = ctx.identifier; //发送者
                        me.toAccount = options.To_Account; //接收者
                        me.fileMd5 = options.fileMd5; //文件MD5
                        me.businessType = options.businessType; //图片或文件的业务类型，群消息:1; c2c消息:2; 个人头像：3; 群头像：4;
                        me.fileType = options.fileType; //文件类型，不填为默认认为上传的是图片；1：图片；2：文件；3：短视频；4：PTT

                        me.cbOk = cbOk; //上传成功回调事件
                        me.cbErr = cbErr; //上传失败回调事件

                        me.reader = new FileReader(); //读取文件对象
                        me.blobSlice = File.prototype.mozSlice || File.prototype.webkitSlice || File.prototype.slice; //file的slice方法,不同浏览器不一样

                        me.reader.onloadstart = me.onLoadStart; //开始读取回调事件
                        me.reader.onprogress = me.onProgress; //读取文件进度回调事件
                        me.reader.onabort = me.onAbort; //停止读取回调事件
                        me.reader.onerror = me.onerror; //读取发生错误回调事件
                        me.reader.onload = me.onLoad; //分片加载完毕回调事件
                        me.reader.onloadend = me.onLoadEnd; //读取文件完毕回调事件
                    },
                    //上传方法
                    upload: function() {
                        var me = file_upload;
                        //读取第一块
                        me.readBlob(0);
                    },
                    onLoadStart: function() {
                        var me = file_upload;
                    },
                    onProgress: function(e) {
                        var me = file_upload;
                        me.loaded += e.loaded;
                        if (me.onProgressCallBack) {
                            me.onProgressCallBack(me.loaded, me.total);
                        }
                    },
                    onAbort: function() {
                        var me = file_upload;
                    },
                    onError: function() {
                        var me = file_upload;
                    },
                    onLoad: function(e) {
                        var me = file_upload;
                        if (e.target.readyState == FileReader.DONE) {
                            var slice_data_base64 = e.target.result;
                            //注意，一定要去除base64编码头部
                            var pos = slice_data_base64.indexOf(",");
                            if (pos != -1) {
                                slice_data_base64 = slice_data_base64.substr(pos + 1);
                            }
                            //封装上传图片接口的请求参数
                            var opt = {
                                'From_Account': me.fromAccount,
                                'To_Account': me.toAccount,
                                'Busi_Id': me.businessType,
                                'File_Type': me.fileType,
                                'File_Str_Md5': me.fileMd5,
                                'PkgFlag': UPLOAD_RES_PKG_FLAG.BASE64_DATA,
                                'File_Size': me.total,
                                'Slice_Offset': me.sliceOffset,
                                'Slice_Size': me.sliceSize,
                                'Slice_Data': slice_data_base64,
                                'Seq': me.seq,
                                'Timestamp': me.timestamp,
                                'Random': me.random
                            };

                            //上传成功的成功回调
                            var succCallback = function(resp) {
                                if (resp.IsFinish == 0) {
                                    me.loaded = resp.Next_Offset;
                                    if (me.loaded < me.total) {
                                        me.readBlob(me.loaded);
                                    } else {
                                        me.loaded = me.total;
                                    }
                                } else {

                                    if (me.cbOk) {
                                        var tempResp = {
                                            'ActionStatus': resp.ActionStatus,
                                            'ErrorCode': resp.ErrorCode,
                                            'ErrorInfo': resp.ErrorInfo,
                                            'File_UUID': resp.File_UUID,
                                            'File_Size': resp.Next_Offset,
                                            'URL_INFO': resp.URL_INFO,
                                            'Download_Flag': resp.Download_Flag
                                        };
                                        if (me.fileType == UPLOAD_RES_TYPE.FILE) { //如果上传的是文件，下载地址需要sdk内部拼接
                                            tempResp.URL_INFO = getFileDownUrl(resp.File_UUID, ctx.identifier, me.file.name);
                                        }
                                        me.cbOk(tempResp);
                                    }
                                }
                                Upload_Retry_Times = 0;
                            };
                            //上传失败的回调
                            var errorCallback = function(resp) {
                                if (Upload_Retry_Times < Upload_Retry_Max_Times) {
                                    Upload_Retry_Times++;
                                    setTimeout(function() {
                                        proto_uploadPic(opt, succCallback, errorCallback);
                                    }, 1000);
                                } else {
                                    me.cbErr(resp);
                                }
                                //me.cbErr
                            };
                            //分片上传图片接口
                            proto_uploadPic(opt, succCallback, errorCallback);
                        }
                    },
                    onLoadEnd: function() {
                        var me = file_upload;
                    },
                    //分片读取文件方法
                    readBlob: function(start) {
                        var me = file_upload;
                        var blob, file = me.file;
                        var end = start + me.step;
                        if (end > me.total) {
                            end = me.total;
                            me.sliceSize = end - start;
                        } else {
                            me.sliceSize = me.step;
                        }
                        me.sliceOffset = start;
                        //根据起始和结束位置，分片读取文件
                        blob = me.blobSlice.call(file, start, end);
                        //将分片的二进制数据转换为base64编码
                        me.reader.readAsDataURL(blob);
                    },
                    abortHandler: function() {
                        var me = file_upload;
                        if (me.reader) {
                            me.reader.abort();
                        }
                    }
                };

                //读取文件MD5
                getFileMD5(options.file,
                    function(fileMd5) {
                        log.info('fileMd5: ' + fileMd5);
                        options.fileMd5 = fileMd5;
                        //初始化上传参数
                        file_upload.init(options, cbOk, cbErr);
                        //开始上传文件
                        file_upload.upload();
                    },
                    cbErr
                );
            };
        };


    //web im 基础对象

    //常量对象

    //会话类型
    webim.SESSION_TYPE = SESSION_TYPE;

    webim.MSG_MAX_LENGTH = MSG_MAX_LENGTH;

    //c2c消息子类型
    webim.C2C_MSG_SUB_TYPE = C2C_MSG_SUB_TYPE;

    //群消息子类型
    webim.GROUP_MSG_SUB_TYPE = GROUP_MSG_SUB_TYPE;

    //消息元素类型
    webim.MSG_ELEMENT_TYPE = MSG_ELEMENT_TYPE;

    //群提示消息类型
    webim.GROUP_TIP_TYPE = GROUP_TIP_TYPE;

    //图片类型
    webim.IMAGE_TYPE = IMAGE_TYPE;

    //群系统消息类型
    webim.GROUP_SYSTEM_TYPE = GROUP_SYSTEM_TYPE;

    //好友系统通知子类型
    webim.FRIEND_NOTICE_TYPE = FRIEND_NOTICE_TYPE;

    //群提示消息-群资料变更类型
    webim.GROUP_TIP_MODIFY_GROUP_INFO_TYPE = GROUP_TIP_MODIFY_GROUP_INFO_TYPE;

    //浏览器信息
    webim.BROWSER_INFO = BROWSER_INFO;

    //表情对象
    webim.Emotions = webim.EmotionPicData = emotions;
    //表情标识符和index Map
    webim.EmotionDataIndexs = webim.EmotionPicDataIndex = emotionDataIndexs;

    //腾讯登录服务错误码(托管模式)
    webim.TLS_ERROR_CODE = TLS_ERROR_CODE;

    //连接状态
    webim.CONNECTION_STATUS = CONNECTION_STATUS;

    //上传图片业务类型
    webim.UPLOAD_PIC_BUSSINESS_TYPE = UPLOAD_PIC_BUSSINESS_TYPE;

    //最近联系人类型
    webim.RECENT_CONTACT_TYPE = RECENT_CONTACT_TYPE;

    //上传资源类型
    webim.UPLOAD_RES_TYPE = UPLOAD_RES_TYPE;


    /**************************************/

    //类对象
    //
    //工具对象
    webim.Tool = tool;
    //控制台打印日志对象
    webim.Log = log;

    //消息对象
    webim.Msg = Msg;
    //会话对象
    webim.Session = Session;
    //会话存储对象
    webim.MsgStore = {
        sessMap: function() {
            return MsgStore.sessMap();
        },
        sessCount: function() {
            return MsgStore.sessCount();
        },
        sessByTypeId: function(type, id) {
            return MsgStore.sessByTypeId(type, id);
        },
        delSessByTypeId: function(type, id) {
            return MsgStore.delSessByTypeId(type, id);
        },
        resetCookieAndSyncFlag: function() {
            return MsgStore.resetCookieAndSyncFlag();
        }
    };

    webim.Resources = Resources;

    /**************************************/

    // webim API impl
    //
    //基本接口
    //登录
    webim.login = webim.init = function(loginInfo, listeners, opts, cbOk, cbErr) {

        //初始化连接状态回调函数
        ConnManager.init(listeners.onConnNotify, cbOk, cbErr);

        //设置ie9以下浏览器jsonp回调
        if (listeners.jsonpCallback) jsonpCallback = listeners.jsonpCallback;
        //登录
        _login(loginInfo, listeners, opts, cbOk, cbErr);
    };
    //登出
    //需要传长轮询id
    //这样登出之后其他的登录实例还可以继续收取消息
    webim.logout = webim.offline = function(cbOk, cbErr) {
        return proto_logout('instance', cbOk, cbErr);
    };

    //登出
    //这种登出方式，所有的实例都将不会收到消息推送，直到重新登录
    webim.logoutAll = function(cbOk, cbErr) {
        return proto_logout('all', cbOk, cbErr);
    };


    //消息管理接口
    //发消息接口（私聊和群聊）
    webim.sendMsg = function(msg, cbOk, cbErr) {
        return MsgManager.sendMsg(msg, cbOk, cbErr);
    };
    //拉取未读c2c消息
    webim.syncMsgs = function(cbOk, cbErr) {
        return MsgManager.syncMsgs(cbOk, cbErr);
    };
    //拉取C2C漫游消息
    webim.getC2CHistoryMsgs = function(options, cbOk, cbErr) {
        return MsgManager.getC2CHistoryMsgs(options, cbOk, cbErr);
    };
    //拉取群漫游消息
    webim.syncGroupMsgs = function(options, cbOk, cbErr) {
        return MsgManager.syncGroupMsgs(options, cbOk, cbErr);
    };

    //上报c2c消息已读
    webim.c2CMsgReaded = function(options, cbOk, cbErr) {
        return MsgStore.c2CMsgReaded(options, cbOk, cbErr);
    };

    //上报群消息已读
    webim.groupMsgReaded = function(options, cbOk, cbErr) {
        return proto_groupMsgReaded(options, cbOk, cbErr);
    };

    //设置聊天会话自动标记已读
    webim.setAutoRead = function(selSess, isOn, isResetAll) {
        return MsgStore.setAutoRead(selSess, isOn, isResetAll);
    };

    //群组管理接口
    //
    //创建群
    webim.createGroup = function(options, cbOk, cbErr) {
        return proto_createGroup(options, cbOk, cbErr);
    };
    //创建群-高级接口
    webim.createGroupHigh = function(options, cbOk, cbErr) {
        return proto_createGroupHigh(options, cbOk, cbErr);
    };
    //申请加群
    webim.applyJoinGroup = function(options, cbOk, cbErr) {
        return proto_applyJoinGroup(options, cbOk, cbErr);
    };
    //处理加群申请(同意或拒绝)
    webim.handleApplyJoinGroupPendency = function(options, cbOk, cbErr) {
        return proto_handleApplyJoinGroupPendency(options, cbOk, cbErr);
    };

    //获取群组未决列表
    webim.getPendencyGroup = function(options, cbOk, cbErr) {
        return proto_getPendencyGroup(options, cbOk, cbErr);
    };

    //群未决已读上报
    webim.getPendencyGroupRead = function(options, cbOk, cbErr) {
        return proto_getPendencyGroupRead(options, cbOk, cbErr);
    };

    //处理邀请进群申请(同意或拒绝)
    webim.handleInviteJoinGroupRequest = function(options, cbOk, cbErr) {
        return proto_handleInviteJoinGroupRequest(options, cbOk, cbErr);
    };

    //删除加群申请
    webim.deleteApplyJoinGroupPendency = function(options, cbOk, cbErr) {
        return proto_deleteC2CMsg(options, cbOk, cbErr);
    };

    //主动退群
    webim.quitGroup = function(options, cbOk, cbErr) {
        return proto_quitGroup(options, cbOk, cbErr);
    };
    //搜索群组(根据名称)
    webim.searchGroupByName = function(options, cbOk, cbErr) {
        return proto_searchGroupByName(options, cbOk, cbErr);
    };
    //获取群组公开资料(根据群id搜索)
    webim.getGroupPublicInfo = function(options, cbOk, cbErr) {
        return proto_getGroupPublicInfo(options, cbOk, cbErr);
    };
    //获取群组详细资料-高级接口
    webim.getGroupInfo = function(options, cbOk, cbErr) {
        return proto_getGroupInfo(options, cbOk, cbErr);
    };
    //修改群基本资料
    webim.modifyGroupBaseInfo = function(options, cbOk, cbErr) {
        return proto_modifyGroupBaseInfo(options, cbOk, cbErr);
    };
    //获取群成员列表
    webim.getGroupMemberInfo = function(options, cbOk, cbErr) {
        return proto_getGroupMemberInfo(options, cbOk, cbErr);
    };
    //邀请好友加群
    webim.addGroupMember = function(options, cbOk, cbErr) {
        return proto_addGroupMember(options, cbOk, cbErr);
    };
    //修改群成员资料
    webim.modifyGroupMember = function(options, cbOk, cbErr) {
        return proto_modifyGroupMember(options, cbOk, cbErr);
    };
    //删除群成员
    webim.deleteGroupMember = function(options, cbOk, cbErr) {
        return proto_deleteGroupMember(options, cbOk, cbErr);
    };
    //解散群
    webim.destroyGroup = function(options, cbOk, cbErr) {
        return proto_destroyGroup(options, cbOk, cbErr);
    };
    //转让群组
    webim.changeGroupOwner = function(options, cbOk, cbErr) {
        return proto_changeGroupOwner(options, cbOk, cbErr);
    };

    //获取我的群组列表-高级接口
    webim.getJoinedGroupListHigh = function(options, cbOk, cbErr) {
        return proto_getJoinedGroupListHigh(options, cbOk, cbErr);
    };
    //获取群成员角色
    webim.getRoleInGroup = function(options, cbOk, cbErr) {
        return proto_getRoleInGroup(options, cbOk, cbErr);
    };
    //设置群成员禁言时间
    webim.forbidSendMsg = function(options, cbOk, cbErr) {
        return proto_forbidSendMsg(options, cbOk, cbErr);
    };
    //发送自定义群系统通知
    webim.sendCustomGroupNotify = function(options, cbOk, cbErr) {
        return proto_sendCustomGroupNotify(options, cbOk, cbErr);
    };

    //进入大群
    webim.applyJoinBigGroup = function(options, cbOk, cbErr) {
        return proto_applyJoinBigGroup(options, cbOk, cbErr);
    };
    //退出大群
    webim.quitBigGroup = function(options, cbOk, cbErr) {
        return proto_quitBigGroup(options, cbOk, cbErr);
    };

    //资料关系链管理接口
    //
    //获取个人资料接口，可用于搜索用户
    webim.getProfilePortrait = function(options, cbOk, cbErr) {
        return proto_getProfilePortrait(options, cbOk, cbErr);
    };
    //设置个人资料
    webim.setProfilePortrait = function(options, cbOk, cbErr) {
        return proto_setProfilePortrait(options, cbOk, cbErr);
    };
    //申请加好友
    webim.applyAddFriend = function(options, cbOk, cbErr) {
        return proto_applyAddFriend(options, cbOk, cbErr);
    };
    //获取好友申请列表
    webim.getPendency = function(options, cbOk, cbErr) {
        return proto_getPendency(options, cbOk, cbErr);
    };
    //好友申请列表已读上报
    webim.getPendencyReport = function(options, cbOk, cbErr) {
        return proto_getPendencyReport(options, cbOk, cbErr);
    };
    //删除好友申请
    webim.deletePendency = function(options, cbOk, cbErr) {
        return proto_deletePendency(options, cbOk, cbErr);
    };
    //处理好友申请
    webim.responseFriend = function(options, cbOk, cbErr) {
        return proto_responseFriend(options, cbOk, cbErr);
    };
    //获取我的好友
    webim.getAllFriend = function(options, cbOk, cbErr) {
        return proto_getAllFriend(options, cbOk, cbErr);
    };
    //删除会话
    webim.deleteChat = function(options, cbOk, cbErr) {
        return proto_deleteChat(options, cbOk, cbErr);
    };
    //删除好友
    webim.deleteFriend = function(options, cbOk, cbErr) {
        return proto_deleteFriend(options, cbOk, cbErr);
    };
    //拉黑
    webim.addBlackList = function(options, cbOk, cbErr) {
        return proto_addBlackList(options, cbOk, cbErr);
    };
    //删除黑名单
    webim.deleteBlackList = function(options, cbOk, cbErr) {
        return proto_deleteBlackList(options, cbOk, cbErr);
    };
    //获取我的黑名单
    webim.getBlackList = function(options, cbOk, cbErr) {
        return proto_getBlackList(options, cbOk, cbErr);
    };

    //获取最近会话
    webim.getRecentContactList = function(options, cbOk, cbErr) {
        return proto_getRecentContactList(options, cbOk, cbErr);
    };

    //图片或文件服务接口
    //
    //上传文件接口（高版本浏览器）
    webim.uploadFile = webim.uploadPic = function(options, cbOk, cbErr) {
        return FileUploader.uploadFile(options, cbOk, cbErr);
    };
    //提交上传图片表单接口（用于低版本ie）
    webim.submitUploadFileForm = function(options, cbOk, cbErr) {
        return FileUploader.submitUploadFileForm(options, cbOk, cbErr);
    };
    //上传图片或文件(Base64)接口
    webim.uploadFileByBase64 = webim.uploadPicByBase64 = function(options, cbOk, cbErr) {
        //请求参数
        var opt = {
            'To_Account': options.toAccount,
            'Busi_Id': options.businessType,
            'File_Type': options.File_Type,
            'File_Str_Md5': options.fileMd5,
            'PkgFlag': UPLOAD_RES_PKG_FLAG.BASE64_DATA,
            'File_Size': options.totalSize,
            'Slice_Offset': 0,
            'Slice_Size': options.totalSize,
            'Slice_Data': options.base64Str,
            'Seq': nextSeq(),
            'Timestamp': unixtime(),
            'Random': createRandom()
        };
        return proto_uploadPic(opt, cbOk, cbErr);
    };

    //设置jsonp返回的值
    webim.setJsonpLastRspData = function(rspData) {
        jsonpLastRspData = typeof(rspData) == "string" ? JSON.parse(rspData) : rspData;
    };

    //获取长轮询ID
    webim.getLongPollingId = function(options, cbOk, cbErr) {
        return proto_getLongPollingId(options, cbOk, cbErr);
    };

    //获取下载地址
    webim.applyDownload = function(options, cbOk, cbErr) {
        return proto_applyDownload(options, cbOk, cbErr);
    };

    //获取下载地址
    webim.onDownFile = function(uuid) {
        window.open(Resources.downloadMap["uuid_" + uuid]);
    };

    //检查是否登录
    webim.checkLogin = function(cbErr, isNeedCallBack) {
        return checkLogin(cbErr, isNeedCallBack);
    };
})(webim);