package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.storage.DefaultStorageManagerFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

public class ExtValCoreConfigurationCustomStorageManagerFactoryClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationCustomStorageManagerFactoryClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomDefaultStorageManagerFactory extends
            DefaultStorageManagerFactory
    {

    }

    public static class Custom2DefaultStorageManagerFactory extends
            DefaultStorageManagerFactory
    {

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_STORAGE_MANAGER_FACTORY",
                    CustomDefaultStorageManagerFactory.class.getName());

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
                public String customStorageManagerFactoryClassName()
                {

                    return Custom2DefaultStorageManagerFactory.class.getName();
                }

            }, true);
        }
    }

    public void testCustomStorageManagerFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.STORAGE_MANAGER_FACTORY, Object.class);
        assertEquals(DefaultStorageManagerFactory.class.getName(), factory
                .getClass().getName());
    }

    public void testCustomStorageManagerFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.STORAGE_MANAGER_FACTORY, Object.class);
        assertEquals(CustomDefaultStorageManagerFactory.class.getName(),
                factory.getClass().getName());
    }

    public void testCustomStorageManagerFactoryClassNameCustomConfig()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.STORAGE_MANAGER_FACTORY, Object.class);
        assertEquals(Custom2DefaultStorageManagerFactory.class.getName(),
                factory.getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomStorageManagerFactoryClassNameTestCase.class);
    }

}
