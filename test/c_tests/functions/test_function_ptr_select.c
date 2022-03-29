int a(void)
{
  return 6;
}

int b(void)
{
	return 0;
}

int main(int argc, char **argv)
{
	int (*fp)(void) = (argc > 1 ? b : a);
	return fp();
}
