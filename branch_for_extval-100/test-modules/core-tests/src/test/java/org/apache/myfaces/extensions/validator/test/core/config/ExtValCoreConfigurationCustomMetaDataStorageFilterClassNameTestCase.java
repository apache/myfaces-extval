package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.storage.DefaultMetaDataStorage;
import org.apache.myfaces.extensions.validator.core.storage.MetaDataStorageFilter;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import java.lang.reflect.Field;
import java.util.List;

public class ExtValCoreConfigurationCustomMetaDataStorageFilterClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationCustomMetaDataStorageFilterClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomMetaDataStorageFilter implements
            MetaDataStorageFilter
    {

        public void filter(PropertyInformation propertyInformation)
        {

        }

    }

    public static class Custom2MetaDataStorageFilter implements
            MetaDataStorageFilter
    {

        public void filter(PropertyInformation propertyInformation)
        {

        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_META_DATA_STORAGE_FILTER",
                    CustomMetaDataStorageFilter.class.getName());
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
                public String customMetaDataStorageFilterClassName()
                {
                    return Custom2MetaDataStorageFilter.class.getName();
                }

            }, true);
        }
    }

    public void testCustomMetaDataStorageFilterClassNameDefault()
            throws Exception
    {
        DefaultMetaDataStorage metaDataStorage = new DefaultMetaDataStorage();
        // A little reflection stuff because filters can not be exposed in any
        // way.
        Field field = DefaultMetaDataStorage.class
                .getDeclaredField("metaDataStorageFilters");
        field.setAccessible(true);
        Object data = field.get(metaDataStorage);
        assertNotNull(data);
        List<MetaDataStorageFilter> metaDataStorageFilters = (List<MetaDataStorageFilter>) data;
        assertEquals(0, metaDataStorageFilters.size());
    }

    public void testCustomMetaDataStorageFilterClassNameWebXml()
            throws Exception
    {
        DefaultMetaDataStorage metaDataStorage = new DefaultMetaDataStorage();
        // A little reflection stuff because filters can not be exposed in any
        // way.
        Field field = DefaultMetaDataStorage.class
                .getDeclaredField("metaDataStorageFilters");
        field.setAccessible(true);
        Object data = field.get(metaDataStorage);
        assertNotNull(data);
        List<MetaDataStorageFilter> metaDataStorageFilters = (List<MetaDataStorageFilter>) data;
        assertEquals(1, metaDataStorageFilters.size());
        assertEquals(CustomMetaDataStorageFilter.class.getName(),
                metaDataStorageFilters.get(0).getClass().getName());
    }

    public void testCustomMetaDataStorageFilterClassNameCustomConfig()
            throws Exception
    {
        DefaultMetaDataStorage metaDataStorage = new DefaultMetaDataStorage();
        // A little reflection stuff because filters can not be exposed in any
        // way.
        Field field = DefaultMetaDataStorage.class
                .getDeclaredField("metaDataStorageFilters");
        field.setAccessible(true);
        Object data = field.get(metaDataStorage);
        assertNotNull(data);
        List<MetaDataStorageFilter> metaDataStorageFilters = (List<MetaDataStorageFilter>) data;
        assertEquals(1, metaDataStorageFilters.size());
        assertEquals(Custom2MetaDataStorageFilter.class.getName(),
                metaDataStorageFilters.get(0).getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomMetaDataStorageFilterClassNameTestCase.class);
    }

}
