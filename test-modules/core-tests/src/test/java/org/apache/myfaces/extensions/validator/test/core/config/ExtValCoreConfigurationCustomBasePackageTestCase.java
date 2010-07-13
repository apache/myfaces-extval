package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.CustomInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

public class ExtValCoreConfigurationCustomBasePackageTestCase extends
        ExtValCoreConfigurationTestCase
{

    private static final String ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_TEST = "org.apache.myfaces.extensions.validator.test.";
    private static final String ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_CONFIG = "org.apache.myfaces.extensions.validator.config.";

    public ExtValCoreConfigurationCustomBasePackageTestCase(String name)
    {
        super(name);

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_BASE_PACKAGE",
                    ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_TEST);
        }
    }

    @Override
    protected void invokeStartupListeners()
    {
        super.invokeStartupListeners();

        // This is the normal place where the user can intervene in the startup
        // and change the config
        if (needCustomConfig())
        {

            ExtValCoreConfiguration.use(new DefaultExtValCoreConfiguration()
            {

                @Override
                public String customBasePackage()
                {
                    return ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_CONFIG;
                }

            }, true);
        }
    }

    public void testCustomBasePackageDefault() throws Exception
    {
        String value = ExtValContext.getContext().getInformationProviderBean()
                .get(CustomInformation.BASE_PACKAGE);
        assertEquals("org.apache.myfaces.extensions.validator.custom.", value);
    }

    public void testCustomBasePackageWebXml()
    {

        String value = ExtValContext.getContext().getInformationProviderBean()
                .get(CustomInformation.BASE_PACKAGE);
        assertEquals(ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_TEST, value);
    }

    public void testCustomBasePackageCustomConfig()
    {

        String value = ExtValContext.getContext().getInformationProviderBean()
                .get(CustomInformation.BASE_PACKAGE);
        assertEquals(ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_CONFIG, value);
    }

    public static Test suite()
    {
        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomBasePackageTestCase.class);
    }

}
