package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.DefaultMetaDataTransformerFactory;
import org.apache.myfaces.extensions.validator.test.base.mock.MockMetaDataTransformerFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

public class ExtValCoreConfigurationCustomMetaDataTransformerFactoryClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationCustomMetaDataTransformerFactoryClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomMetaDataTransformerFactory extends
            DefaultMetaDataTransformerFactory
    {

    }

    public static class Custom2MetaDataTransformerFactory extends
            DefaultMetaDataTransformerFactory
    {

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_META_DATA_TRANSFORMER_FACTORY",
                    CustomMetaDataTransformerFactory.class.getName());

        }
    }

    public void testCustomMetaDataTransformerFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.META_DATA_TRANSFORMER_FACTORY, Object.class);

        // The TestCase setup registers a Mockfactory so that a protected method
        // is made visible.
        // assertEquals(DefaultMetaDataTransformerFactory.class.getName(),
        // factory
        // .getClass().getName());
        assertEquals(MockMetaDataTransformerFactory.class.getName(), factory
                .getClass().getName());
    }

    public void testCustomMetaDataTransformerFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.META_DATA_TRANSFORMER_FACTORY, Object.class);

        assertEquals(CustomMetaDataTransformerFactory.class.getName(), factory
                .getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomMetaDataTransformerFactoryClassNameTestCase.class);
    }

}
