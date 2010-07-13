package org.apache.myfaces.extensions.validator.test.core.config;

import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;

public abstract class ExtValCoreConfigurationTestCase extends
        AbstractExValCoreTestCase
{

    public ExtValCoreConfigurationTestCase(String name)
    {
        super(name);
    }

    protected boolean needXmlParameters()
    {
        return getName().contains("Xml");
    }

    protected boolean needCustomConfig()
    {
        return !getName().contains("Xml") && !getName().contains("Default");
    }

}
