extern int ARGC;
extern char **ARGV;

extern int harlan_main();

int main(int argc, char **argv) {
	ARGC = argc;
	ARGV = argv;

	return harlan_main();
}
