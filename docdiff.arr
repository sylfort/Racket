
#Example input to overap:
doc11 = [list: "a", "B", "c"]
doc12 = [list: "d", "d", "D", "b"]

#Distinct/lowercase and sorted combination of doc1/doc2
doc1-plus-doc2 = [list: "a", "b", "c", "d"]

#Total words:[list: "a", "b", "c", "d"]
doc1-vec =  [list:  1,   1,   1,   0]
doc2-vec =  [list:  0,   1,   0,   3]

fun frequencies(doc1-doc2-joined :: List<String>, docX :: List<String>) -> List<Number>:
  doc: "create a vector of frequencies of words in doc in doc1-doc2-joined"
  cases (List) doc1-doc2-joined:
  |empty => empty
  |link(first, rest) =>
    if docX.member(first):
      link((docX.filter(lam(x): x == first end)).length(), (frequencies(rest, docX.filter(lam(x): not(x == first) end))))
    else:
      link(0, frequencies(rest, docX))
    end
  end
      
  where:
    frequencies([list: "a", "b", "c"], [list: "d"]) is [list: 0, 0, 0]
    # Property-based test where 2 similar words should be [list: 0, 2, 0]
    frequencies([list: "a", "b", "c"], [list: "b", "b"])
      .foldl(lam(elt, acc): acc + elt end, 0) is 2
    # Property-based test where all similar words should be length of vec
    frequencies([list: "a", "b", "c"], [list: "a", "a", "a"]).length() is 3
  end


fun dot-product(vec1 :: List<Number>, vec2 :: List<Number>) -> List<Number>:
doc: "calculate the dot product between two lists"
  cases (List) vec1:
  |empty => empty
  |link(first, rest) =>
      if (first == 0):
      link(0, dot-product(rest, vec2.rest))
    else:
        link(first * vec2.first, dot-product(rest, vec2.rest))
    end
  end

where:
  dot-product([list: 1, 2, 3], [list: 4, 5, 6]) is [list: 4, 10, 18]
  dot-product([list: 0, 2, 0], [list: 4, 5, 6]) is [list: 0, 10, 0]
  dot-product([list: 1, 1, 1], [list: 4, 5, 6]) is [list: 4, 5, 6]
  end

fun overlap(doc1 :: List<String>, doc2 :: List<String>) -> Number:
  doc: "calculate the overlap between two documents"
  #lowercase
  lower-doc1 = map(string-to-lower, doc1)
  lower-doc2 = map(string-to-lower, doc2)
  #Distinct/lowercase and sorted combination of doc1/doc2
  doc1-doc2-joined  = sort(lower-doc1 + lower-doc2)
  
  #dot product
  doc1-freq = frequencies(doc1-doc2-joined, lower-doc1)
  doc2-freq = frequencies(doc1-doc2-joined, lower-doc2)
  #use fold to sum the products of the elements of the two vectors
  dot-product-list = dot-product(doc1-freq, doc2-freq)
  #reduce the dot product list to a single number
  dot-product-reduced = dot-product-list.foldl(lam(elt, acc): acc + elt end, 0)
  #calculate the magnitude of the two vectors
  #|  The magnitude (or length) of a vector is a measure of how "long" the vector is.
 It's calculated by squaring each component of the vector, summing those squares, 
 and then taking the square root of that sum. In this context, 
 it's a measure of how many words are in the document 
 (with more frequent words counted multiple times).
 |#
  doc1-mag = (num-sqrt(num-sqr(doc1-freq)))
  doc2-mag = (num-sqrt(num-sqr(doc2-freq)))

  (dot-product-reduced / num-max(doc1-mag, doc2-mag))

  end



#|  The magnitude (or length) of a vector is a measure of how "long" the vector is.
 It's calculated by squaring each component of the vector, summing those squares, 
 and then taking the square root of that sum. In this context, 
 it's a measure of how many words are in the document 
 (with more frequent words counted multiple times).
 |#

