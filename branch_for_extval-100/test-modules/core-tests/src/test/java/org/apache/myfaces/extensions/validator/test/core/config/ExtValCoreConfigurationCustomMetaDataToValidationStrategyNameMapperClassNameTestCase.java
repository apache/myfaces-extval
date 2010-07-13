package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.factory.NameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.test.base.mock.MockValidationStrategyFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import java.util.List;

public class ExtValCoreConfigurationCustomMetaDataToValidationStrategyNameMapperClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{
    private static final String WEB_XML = "Web.xml";
    private static final String CUSTOM_CONFIG = "Custom Config";

    public ExtValCoreConfigurationCustomMetaDataToValidationStrategyNameMapperClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomNameMapper implements NameMapper<String>
    {

        public String createName(String source)
        {
            return WEB_XML;
        }

    }

    public static class Custom2NameMapper implements NameMapper<String>
    {

        public String createName(String source)
        {
            return CUSTOM_CONFIG;
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_META_DATA_TO_VALIDATION_STRATEGY_NAME_MAPPER",
                    CustomNameMapper.class.getName());
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
                public String customMetaDataToValidationStrategyNameMapperClassName()
                {
                    return Custom2NameMapper.class.getName();
                }

            }, true);
        }
    }

    public void testCustomMetaDataToValidationStrategyNameMapperClassNameDefault()
    {
        List<NameMapper<String>> nameMappers = getNameMappers();
        assertEquals(8, nameMappers.size());
        // The first one (due to @InvocationOrder) is the
        // CustomConfiguredAnnotationToValidationStrategyNameMapper which we can
        // customize and testing here.
        NameMapper<String> mapper = nameMappers.get(0);
        // By default nothing is configures so should return null.
        assertNull(mapper.createName(null));

    }

    public void testCustomMetaDataToValidationStrategyNameMapperClassNameWebXml()
    {
        List<NameMapper<String>> nameMappers = getNameMappers();
        // No mapper additional, but the first mapper contain now our custom
        // configured mapper.
        assertEquals(8, nameMappers.size());
        NameMapper<String> mapper = nameMappers.get(0);
        // So now it should return some value.
        assertEquals(WEB_XML, mapper.createName(null));
    }

    public void testCustomMetaDataToValidationStrategyNameMapperClassNameCustomConfig()
    {
        List<NameMapper<String>> nameMappers = getNameMappers();
        // No mapper additional, but the first mapper contain now our custom
        // configured mapper.
        assertEquals(8, nameMappers.size());
        NameMapper<String> mapper = nameMappers.get(0);
        // So now it should return some value.
        assertEquals(CUSTOM_CONFIG, mapper.createName(null));
    }

    private List<NameMapper<String>> getNameMappers()
    {
        NameMapperAwareFactory result = ExtValContext.getContext()
                .getFactoryFinder().getFactory(
                        FactoryNames.VALIDATION_STRATEGY_FACTORY,
                        NameMapperAwareFactory.class);

        return ((MockValidationStrategyFactory) result)
                .getRegisteredNameMapperList();
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomMetaDataToValidationStrategyNameMapperClassNameTestCase.class);
    }

}
