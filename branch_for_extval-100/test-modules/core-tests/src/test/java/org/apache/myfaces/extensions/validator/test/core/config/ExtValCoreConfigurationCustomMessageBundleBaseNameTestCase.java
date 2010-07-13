package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.DefaultValidationErrorMessageResolver;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

public class ExtValCoreConfigurationCustomMessageBundleBaseNameTestCase extends
        ExtValCoreConfigurationTestCase
{

    private static final String ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_TEST = "org.apache.myfaces.extensions.validator.test.";
    private static final String ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_CONFIG = "org.apache.myfaces.extensions.validator.config.";

    static class VisibleDefaultValidationErrorMessageResolver extends
            DefaultValidationErrorMessageResolver
    {
        public String getCustomBaseName()
        {
            return super.getCustomBaseName();

        }
    }

    ;

    public ExtValCoreConfigurationCustomMessageBundleBaseNameTestCase(
            String name)
    {
        super(name);
    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_MESSAGE_BUNDLE",
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
                public String customMessageBundleBaseName()
                {

                    return ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_CONFIG;
                }

            }, true);
        }
    }

    public void testCustomMessageBundleBaseNameDefault() throws Exception
    {
        VisibleDefaultValidationErrorMessageResolver messageResolver = new VisibleDefaultValidationErrorMessageResolver();
        assertNull(messageResolver.getCustomBaseName());

    }

    public void testCustomMessageBundleBaseNameWebXml() throws Exception
    {

        VisibleDefaultValidationErrorMessageResolver messageResolver = new VisibleDefaultValidationErrorMessageResolver();

        assertEquals(ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_TEST,
                messageResolver.getCustomBaseName());
    }

    public void testCustomMessageBundleBaseNameCustomConfig() throws Exception
    {
        VisibleDefaultValidationErrorMessageResolver messageResolver = new VisibleDefaultValidationErrorMessageResolver();

        assertEquals(ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_CONFIG,
                messageResolver.getCustomBaseName());

    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomMessageBundleBaseNameTestCase.class);
    }

}
