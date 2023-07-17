#Example input to overap:
doc1 = [list: "a", "B", "c"]
doc2 = [list: "d", "d", "D", "b"]

#Distinct/lowercase and sorted combination of doc1/doc2
doc1 + doc2 [list: "a", "b", "c", "d"]

#Total words:[list: "a", "b", "c", "d"]
doc1-vec =  [list:  1,   1,   1,   0]
doc2-vec =  [list:  0,   1,   0,   3]

fun frequencies(doc :: List<String>) -> List<Number>:
  #compute word frequency in doc
  cases (List) doc:
  | empty => empty
  |
    where:
    #these examples taken from the Examplar paper

  end

fun dot-product(vec1 :: List<String, vec2 :: List<String>) -> 

fun overlap(doc1 :: List<String>, doc2 :: List<String>) -> Number:
  doc: "Computes the overlap between doc1 and doc2"
  if (doc1 == empty) or (doc2 == empty):
    raise("Empty input")
  else:
    #lowercase
    lower-doc1 = map(string-to-lower, doc1)
    lower-doc2 = map(string-to-lower, doc2)
    #compute word frequencies
    freq1 = frequencies(lower-doc1)
    freq2 = frequencies(lower-doc2)
    combined = distinct((lower-doc1 + lower-doc2).sort())
    #dot product
    (dot-product(freq1, freq2) / num-max(
      num-sqr(num-sqrt(dot-product(freq1, freq1))),
      num-sqr(num-sqrt(dot-product(freq1, freq1)))))
  end
  
where:
 # these examples taken from the Examplar paper
  overlap([list: "welcome", "to", "Walmart"], 
    [list: "WELCOME", "To", "walmart"]) is-roughly 3/3
  overlap([list: "1", "!", "A", "?", "b"], 
    [list: "1", "A", "b"]) is-roughly 3/5
  overlap([list: "alakazam", "abra"],
    [list: "abra", "kadabra", "alakazam", "abra"]) is-roughly 2/4
  overlap([list: "a", "b"], [list: "c"]) is 0/3

  # epsilon test for roughnums
  epsilon = 0.001
  a = [list: "alakazam", "abra"]
  b = [list: "abra", "kadabra", "alakazam", "abra"]

  num-abs(overlap(a, b) - 2/4) <= epsilon is true  
end

#|  The magnitude (or length) of a vector is a measure of how "long" the vector is.
 It's calculated by squaring each component of the vector, summing those squares, 
 and then taking the square root of that sum. In this context, 
 it's a measure of how many words are in the document 
 (with more frequent words counted multiple times).
 |#

