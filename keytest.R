
repeat {
    k <- keypress::keypress(FALSE)
    if (is.na(k)) next
    if (k == "q") break
    print(k)
}