/**
 * Builds a pair of DFAs which can be used to match 2D patterns. The `rowDFA`
 * recognises pattern rows, and the `colDFA` recognises sequences of pattern
 * rows matched by the `rowDFA`.
 * 
 * The DFAs recognise the patterns in reverse order, for convenience so that
 * matches are reported where the patterns start rather than where they end.
 */
function makePatternMatcherDFAs(alphabetSize: number, patterns: ReadonlyIDMap<Pattern>): [DFA, DFA] {
    const numPatterns = patterns.size();
    
    const rowPatterns = IDMap.ofWithKey(patterns.map(Pattern.rowsOf).flat(), Pattern.key);
    const rowRegex = Regex.concat([
        Regex.kleeneStar(Regex.wildcard()),
        Regex.union(
            rowPatterns.map((row, rowID) => Regex.concat([
                Regex.concat(row.masks.map(c => Regex.letters(ISet.toArray(c))).reverse()),
                Regex.accept(rowID),
            ]))
        ),
    ]);
    const rowDFA = Regex.compile(alphabetSize, rowPatterns.size(), rowRegex);
    
    const acceptingSets: number[][] = makeArray(rowPatterns.size(), () => []);
    rowDFA.acceptSetMap.forEach((xs, id) => {
        for(const x of xs) {
            acceptingSets[x].push(id);
        }
    });
    
    const colRegex = Regex.concat([
        Regex.kleeneStar(Regex.wildcard()),
        Regex.union(
            patterns.map((pattern, patternID) => Regex.concat([
                Regex.concat(Pattern.rowsOf(pattern).map(row => {
                    const rowID = rowPatterns.getID(row);
                    return Regex.letters(acceptingSets[rowID]);
                }).reverse()),
                Regex.accept(patternID),
            ]))
        ),
    ]);
    const colDFA = Regex.compile(rowDFA.acceptSetMap.size(), numPatterns, colRegex);
    
    return [rowDFA, colDFA];
}
