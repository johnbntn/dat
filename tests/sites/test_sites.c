int src() {
    return 0;
}

int dst() {
    return 0;
}

int q() {
    dst();
    return 0;
}

int main() {
    src();
    q();
    return 0;
}

