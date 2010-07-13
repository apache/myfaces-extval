package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.ComponentMetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import java.util.Map;

public class ExtValCoreConfigurationCustomComponentMetaDataExtractorFactoryClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationCustomComponentMetaDataExtractorFactoryClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomComponentMetaDataExtractorFactory implements
            ComponentMetaDataExtractorFactory
    {

        public MetaDataExtractor create()
        {

            return null;
        }

        public MetaDataExtractor createWith(Map<String, Object> properties)
        {

            return null;
        }

    }

    public static class Custom2ComponentMetaDataExtractorFactory implements
            ComponentMetaDataExtractorFactory
    {

        public MetaDataExtractor create()
        {

            return null;
        }

        public MetaDataExtractor createWith(Map<String, Object> properties)
        {

            return null;
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_COMPONENT_META_DATA_EXTRACTOR_FACTORY",
                    CustomComponentMetaDataExtractorFactory.class.getName());

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
                public String customComponentMetaDataExtractorFactoryClassName()
                {
                    return Custom2ComponentMetaDataExtractorFactory.class.getName();
                }

            }, true);
        }
    }

    public void testCustomComponentMetaDataExtractorFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.COMPONENT_META_DATA_EXTRACTOR_FACTORY,
                ComponentMetaDataExtractorFactory.class);
        assertEquals(DefaultComponentMetaDataExtractorFactory.class.getName(),
                factory.getClass().getName());
    }

    public void testCustomComponentMetaDataExtractorFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.COMPONENT_META_DATA_EXTRACTOR_FACTORY,
                ComponentMetaDataExtractorFactory.class);
        assertEquals(CustomComponentMetaDataExtractorFactory.class.getName(),
                factory.getClass().getName());
    }

    public void testCustomComponentMetaDataExtractorFactoryClassNameCustomConfig()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.COMPONENT_META_DATA_EXTRACTOR_FACTORY,
                ComponentMetaDataExtractorFactory.class);
        assertEquals(Custom2ComponentMetaDataExtractorFactory.class.getName(), factory.getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomComponentMetaDataExtractorFactoryClassNameTestCase.class);
    }

}
