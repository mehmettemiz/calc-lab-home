// Mehmet Temiz 251ADB232
// https://github.com/mehmettemiz/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources: lecture pdfs and old lab works done so far
#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <dirent.h>
#include <sys/stat.h>
 
#define NAME "mehmet"
#define LASTNAME "temiz"
#define STUDENTID "251ADB232"
 
// value is int or a double is float says which one
typedef struct {
    int isFloat;
    long long i;
    double d;
} Value;
 
/* no token list parser is just walks over the buffer with p,
p is an index in the whole file so p + 1 is already the 1-based position
it counts every \n and every comment line without extra work */
static char *text;
static long len;
static long p;
static long lineEnd;
static long errPos = 0;
 
// results are saved in memory because if we get error only the error ma be written
static char *out = NULL;
static size_t outLen = 0;
static size_t outCap = 0;
 
static Value parseExpr(void);
 
static Value makeInt(long long x) {
    Value v = {0, x, 0.0};
    return v;
}
 
static Value makeFloat(double x) {
    Value v = {1, 0, x};
    return v;
}
 
static double toDouble(Value v) {
    if (v.isFloat) {
        return v.d;
    }
    return (double)v.i;
}
 
static int isDigit(char c) {
    return c >= '0' && c <= '9';
}
 
static int isBlank(char c) {
    return c == ' ' || c == '\t' || c == '\r';
}
 
// only the first error is kept because we stopping at the first error
static void setError(long index) {
    if (errPos == 0) {
        errPos = index + 1;
    }
}
 
static void skipSpaces(void) {
    while (p < lineEnd && isBlank(text[p])) {
        p++;
    }
}
 
/* every error is reported and spaces are skipped first
so a missing operand or missing ) at the end of a line is reported at lineEnd
which is the \n or file length + 1 if the last line has no \n */
static Value parsePrimary(void) {
    skipSpaces();
    if (p >= lineEnd) {
        setError(p);
        return makeInt(0);
    }
    char c = text[p];
    if (isDigit(c)) {
        long start = p;
        long long number = 0;
        while (p < lineEnd && isDigit(text[p])) {
            number = number * 10 + (text[p] - '0');
            p++;
        }
        /* its float only if digit comes after the dot so for "1." or "1.5e3"
        the number stops early and the leftover . or e is reported as the errors */
        if (p + 1 < lineEnd && text[p] == '.' && isDigit(text[p + 1])) {
            p++;
            while (p < lineEnd && isDigit(text[p])) {
                p++;
            }
            // strtod needs a \0 at the end so I put one in the buffer 
            char saved = text[p];
            text[p] = '\0';
            double d = strtod(text + start, NULL);
            text[p] = saved;
            return makeFloat(d);
        }
        return makeInt(number);
    }
    if (c == '(') {
        p++;
        Value v = parseExpr();
        if (errPos != 0) {
            return v;
        }
        skipSpaces();
        if (p < lineEnd && text[p] == ')') {
            p++;
        } else {
            setError(p);
        }
        return v;
    }
    // invalid character or an operator where a number should be like no unary operators exist
    setError(p);
    return makeInt(0);
}
 
/* power calls itself for the exponent so 2 ** 3 ** 2 groups as 2 ** (3 ** 2)
this makes ** right-associative base is only a primary
so ** is stronger than * and / (they call the power) */
static Value parsePower(void) {
    Value left = parsePrimary();
    if (errPos != 0) {
        return left;
    }
    skipSpaces();
    if (p + 1 < lineEnd && text[p] == '*' && text[p + 1] == '*') {
        long opPos = p;
        p += 2;
        skipSpaces();
        long expPos = p; // negative exponent is reported where the exponent starts
        Value right = parsePower();
        if (errPos != 0) {
            return right;
        }
        if (!left.isFloat && !right.isFloat) {
            if (right.i < 0) {
                setError(expPos);
                return makeInt(0);
            }
            // have repeated squaring because the exponent can be big unsigned so overflow is not UB
            unsigned long long result = 1;
            unsigned long long base = (unsigned long long)left.i;
            long long e = right.i;
            while (e > 0) {
                if (e % 2 == 1) {
                    result = result * base;
                }
                base = base * base;
                e = e / 2;
            }
            return makeInt((long long)result);
        }
        double r = pow(toDouble(left), toDouble(right));
        if (isinf(r) || isnan(r)) {
            setError(opPos);
            return makeFloat(0);
        }
        return makeFloat(r);
    }
    return left;
}
 
/* integer math used only if both sides are int. If one side is a float everything
is done in double a float result that is inf or nan is an error at the operator */
static Value applyOp(char op, Value a, Value b, long opPos, long rightPos) {
    if (!a.isFloat && !b.isFloat) {
        if (op == '+') {
            return makeInt(a.i + b.i);
        }
        if (op == '-') {
            return makeInt(a.i - b.i);
        }
        if (op == '*') {
            return makeInt(a.i * b.i);
        }
        if (b.i == 0) {
            setError(rightPos); // reported at the start of the right operand
            return makeInt(0);
        }
        return makeInt(a.i / b.i); // C truncates toward zero
    }
    double x = toDouble(a);
    double y = toDouble(b);
    double r;
    if (op == '/' && y == 0.0) {
        setError(rightPos);
        return makeFloat(0);
    }
    if (op == '+') {
        r = x + y;
    } else if (op == '-') {
        r = x - y;
    } else if (op == '*') {
        r = x * y;
    } else {
        r = x / y;
    }
    if (isinf(r) || isnan(r)) {
        setError(opPos);
        return makeFloat(0);
    }
    return makeFloat(r);
}
 
/* term uses a loop so a * b * c is calculated from left to right (left-associative)
calls power so ** is done before * and /. */
static Value parseTerm(void) {
    Value left = parsePower();
    while (errPos == 0) {
        skipSpaces();
        if (p >= lineEnd || (text[p] != '*' && text[p] != '/')) {
            break;
        }
        char op = text[p];
        long opPos = p;
        p++;
        skipSpaces();
        long rightPos = p;
        Value right = parsePower();
        if (errPos != 0) {
            return right;
        }
        left = applyOp(op, left, right, opPos, rightPos);
    }
    return left;
}
 
/* same loop for + and -, it calls term so * and / are done first
a loop also means a very long line like 1+1+1+... doesnt  need deep recursion */
static Value parseExpr(void) {
    Value left = parseTerm();
    while (errPos == 0) {
        skipSpaces();
        if (p >= lineEnd || (text[p] != '+' && text[p] != '-')) {
            break;
        }
        char op = text[p];
        long opPos = p;
        p++;
        skipSpaces();
        long rightPos = p;
        Value right = parseTerm();
        if (errPos != 0) {
            return right;
        }
        left = applyOp(op, left, right, opPos, rightPos);
    }
    return left;
}
 
// the buffer doubles when it is full so adding results is fast
static void appendOut(const char *s) {
    size_t n = strlen(s);
    if (outLen + n > outCap) {
        if (outCap == 0) {
            outCap = 1024;
        }
        while (outLen + n > outCap) {
            outCap = outCap * 2;
        }
        out = realloc(out, outCap);
        if (out == NULL) {
            fprintf(stderr, "Error: out of memory\n");
            exit(1);
        }
    }
    memcpy(out + outLen, s, n);
    outLen += n;
}
 
// goes over the file line by line using only start and end indexes of each line
static void evaluateText(void) {
    long start = 0;
    char buf[64];
    errPos = 0;
    outLen = 0;
    while (start < len && errPos == 0) {
        long end = start;
        while (end < len && text[end] != '\n') {
            end++;
        }
        long q = start;
        while (q < end && isBlank(text[q])) {
            q++;
        }
        // blank lines and lines starting with # gives no output
        if (q < end && text[q] != '#') {
            p = start;
            lineEnd = end;
            Value v = parseExpr();
            skipSpaces();
            if (errPos == 0 && p < lineEnd) {
                setError(p); // something is left: extra ) or invalid character
            }
            if (errPos == 0) {
                if (v.isFloat) {
                    snprintf(buf, sizeof(buf), "%.15g\n", v.d);
                } else {
                    snprintf(buf, sizeof(buf), "%lld\n", v.i);
                }
                appendOut(buf);
            }
        }
        start = end + 1;
    }
    if (errPos != 0) {
        outLen = 0; // no partial results
        snprintf(buf, sizeof(buf), "ERROR:%ld\n", errPos);
        appendOut(buf);
    }
}
 
static int readFile(const char *path) {
    FILE *f = fopen(path, "rb");
    if (f == NULL) {
        return -1;
    }
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    rewind(f);
    if (size < 0) {
        fclose(f);
        return -1;
    }
    text = malloc((size_t)size + 1);
    if (text == NULL) {
        fclose(f);
        return -1;
    }
    size_t got = fread(text, 1, (size_t)size, f);
    fclose(f);
    if (got != (size_t)size) {
        free(text);
        return -1;
    }
    text[size] = '\0';
    len = size;
    return 0;
}
 
// gets the last part of a path
static void getBase(const char *path, char *base, int cutTxt) {
    char tmp[500];
    snprintf(tmp, sizeof(tmp), "%s", path);
    int n = (int)strlen(tmp);
    while (n > 1 && tmp[n - 1] == '/') {
        tmp[n - 1] = '\0';
        n--;
    }
    char *name = tmp;
    char *slash = strrchr(tmp, '/');
    if (slash != NULL) {
        name = slash + 1;
    }
    n = (int)strlen(name);
    if (cutTxt && n > 4 && strcmp(name + n - 4, ".txt") == 0) {
        name[n - 4] = '\0';
    }
    snprintf(base, 500, "%s", name);
}
 
static int makeDir(const char *path) {
    struct stat st;
    if (stat(path, &st) == 0) {
        if (S_ISDIR(st.st_mode)) {
            return 0;
        }
        return -1;
    }
    return mkdir(path, 0755);
}
 
static int processFile(const char *inputPath, const char *outDir) {
    char base[500];
    char outPath[2000];
    getBase(inputPath, base, 1);
    snprintf(outPath, sizeof(outPath), "%s/%s_%s_%s_%s.txt", outDir, base, NAME, LASTNAME, STUDENTID);
    if (readFile(inputPath) != 0) {
        fprintf(stderr, "Error: cannot read %s\n", inputPath);
        return -1;
    }
    evaluateText();
    free(text);
    FILE *f = fopen(outPath, "wb");
    if (f == NULL) {
        fprintf(stderr, "Error: cannot write %s\n", outPath);
        return -1;
    }
    if (outLen > 0) {
        fwrite(out, 1, outLen, f);
    }
    if (fclose(f) != 0) {
        fprintf(stderr, "Error: cannot write %s\n", outPath);
        return -1;
    }
    return 0;
}
 
int main(int argc, char **argv) {
    const char *dirArg = NULL;
    const char *outArg = NULL;
    const char *inputArg = NULL;
    char outDir[1200];
    struct stat st;
    int status = 0;
 
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: missing value for %s\n", argv[i]);
                return 1;
            }
            i++;
            dirArg = argv[i];
        } else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: missing value for %s\n", argv[i]);
                return 1;
            }
            i++;
            outArg = argv[i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Error: unknown option %s\n", argv[i]);
            return 1;
        } else if (inputArg == NULL) {
            inputArg = argv[i];
        } else {
            fprintf(stderr, "Error: only one input file is allowed\n");
            return 1;
        }
    }
    if ((dirArg == NULL) == (inputArg == NULL)) {
        fprintf(stderr, "Usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
        return 1;
    }
 
    // check the input first so a wrong input does not leave an empty output folder
    if (inputArg != NULL) {
        if (stat(inputArg, &st) != 0 || !S_ISREG(st.st_mode)) {
            fprintf(stderr, "Error: input file %s not found\n", inputArg);
            return 1;
        }
    } else {
        if (stat(dirArg, &st) != 0 || !S_ISDIR(st.st_mode)) {
            fprintf(stderr, "Error: folder %s not found\n", dirArg);
            return 1;
        }
    }
 
    // default output folder is the <input_base>_<name>_<studentid>
    if (outArg != NULL) {
        snprintf(outDir, sizeof(outDir), "%s", outArg);
    } else {
        char base[500];
        if (inputArg != NULL) {
            getBase(inputArg, base, 1);
        } else {
            getBase(dirArg, base, 0);
        }
        snprintf(outDir, sizeof(outDir), "%s_%s_%s", base, NAME, STUDENTID);
    }
    if (makeDir(outDir) != 0) {
        fprintf(stderr, "Error: cannot create output folder %s\n", outDir);
        return 1;
    }
 
    if (inputArg != NULL) {
        if (processFile(inputArg, outDir) != 0) {
            status = 1;
        }
    } else {
        DIR *d = opendir(dirArg);
        if (d == NULL) {
            fprintf(stderr, "Error: cannot open folder %s\n", dirArg);
            return 1;
        }
        struct dirent *entry;
        while ((entry = readdir(d)) != NULL) {
            size_t n = strlen(entry->d_name);
            // only *.txt files directly inside DIR the rest is ignored
            if (n <= 4 || strcmp(entry->d_name + n - 4, ".txt") != 0) {
                continue;
            }
            char full[2000];
            snprintf(full, sizeof(full), "%s/%s", dirArg, entry->d_name);
            if (stat(full, &st) == 0 && S_ISREG(st.st_mode)) {
                if (processFile(full, outDir) != 0) {
                    status = 1;
                }
            }
        }
        closedir(d);
    }
    free(out);
    return status;
}
 