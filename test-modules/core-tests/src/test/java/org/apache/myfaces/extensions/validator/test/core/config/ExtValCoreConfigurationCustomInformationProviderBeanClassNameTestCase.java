package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.*;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import java.util.Map;

public class ExtValCoreConfigurationCustomInformationProviderBeanClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationCustomInformationProviderBeanClassNameTestCase(
            String name)
    {
        super(name);

    }

    public static class CustomInformationProviderBean extends
            InformationProviderBean
    {

        @Override
        protected void applyCustomValues(Map<CustomInformation, String> map)
        {
            map.put(CustomInformation.MESSAGE_BUNDLE_NAME, "X");
        }

    }

    public static class CustomInformationProviderBean2 extends
            InformationProviderBean
    {

        @Override
        protected void applyCustomValues(Map<CustomInformation, String> map)
        {
            map.put(CustomInformation.MESSAGE_BUNDLE_NAME, "Y");
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_INFORMATION_PROVIDER_BEAN",
                    CustomInformationProviderBean.class.getName());
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
                public String customInformationProviderBeanClassName()
                {
                    return CustomInformationProviderBean2.class.getName();
                }

            }, true);
        }
    }

    public void testCustomInformationProviderBeanClassNameDefault()
    {
        InformationProviderBean bean = ExtValContext.getContext()
                .getInformationProviderBean();
        assertEquals(InformationProviderBean.class.getName(), bean.getClass()
                .getName());
    }

    public void testCustomInformationProviderBeanClassNameWebXml()
    {
        InformationProviderBean bean = ExtValContext.getContext()
                .getInformationProviderBean();
        assertEquals(CustomInformationProviderBean.class.getName(), bean
                .getClass().getName());
        // An additional test to make sure we have the custom
        // informationProviderBean.
        assertEquals(ExtValInformation.EXTENSIONS_VALIDATOR_BASE_PACKAGE_NAME
                + ".custom.X", bean.get(CustomInformation.MESSAGE_BUNDLE_NAME));
    }

    public void testCustomInformationProviderBeanClassNameCustomConfig()
    {
        InformationProviderBean bean = ExtValContext.getContext()
                .getInformationProviderBean();
        assertEquals(CustomInformationProviderBean2.class.getName(), bean
                .getClass().getName());
        // An additional test to make sure we have the custom
        // informationProviderBean.
        assertEquals(ExtValInformation.EXTENSIONS_VALIDATOR_BASE_PACKAGE_NAME
                + ".custom.Y", bean.get(CustomInformation.MESSAGE_BUNDLE_NAME));
    }

    public static Test suite()
    {
        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomInformationProviderBeanClassNameTestCase.class);
    }

}
