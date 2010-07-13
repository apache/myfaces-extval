package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import java.util.List;

public class ExtValCoreConfigurationCustomMetaDataExtractionInterceptorClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationCustomMetaDataExtractionInterceptorClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomMetaDataExtractionInterceptor implements
            MetaDataExtractionInterceptor
    {

        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }

    }

    public static class Custom2MetaDataExtractionInterceptor implements
            MetaDataExtractionInterceptor
    {

        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_META_DATA_EXTRACTION_INTERCEPTOR",
                    CustomMetaDataExtractionInterceptor.class.getName());
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
                public String customMetaDataExtractionInterceptorClassName()
                {
                    return Custom2MetaDataExtractionInterceptor.class.getName();
                }

            }, true);
        }
    }

    public void testCustomMetaDataExtractionInterceptorClassNameDefault()
    {
        assertEquals(0, ExtValContext.getContext()
                .getMetaDataExtractionInterceptors().size());
    }

    public void testCustomMetaDataExtractionInterceptorClassNameWebXml()
    {
        List<MetaDataExtractionInterceptor> result = ExtValContext.getContext()
                .getMetaDataExtractionInterceptors();
        assertEquals(1, result.size());
        assertEquals(CustomMetaDataExtractionInterceptor.class.getName(),
                result.get(0).getClass().getName());
    }

    public void testCustomMetaDataExtractionInterceptorClassNameCustomConfig()
    {
        List<MetaDataExtractionInterceptor> result = ExtValContext.getContext()
                .getMetaDataExtractionInterceptors();
        assertEquals(1, result.size());
        assertEquals(Custom2MetaDataExtractionInterceptor.class.getName(),
                result.get(0).getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomMetaDataExtractionInterceptorClassNameTestCase.class);
    }

}
