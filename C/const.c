const int g_readonly = 123;

int main(void)
{
	const int readonly = 4335;
	// readonly = 0; // コンパイルエラーになる
	// g_readonly = 0; // コンパイルエラーになる
	return 0;
}
