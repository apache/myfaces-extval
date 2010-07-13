package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.test.core.config.support.ConstraintSourceAwareBean;
import org.apache.myfaces.extensions.validator.test.core.config.support.CustomTargetProperty;
import org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils;
import org.apache.myfaces.extensions.validator.util.ExtValAnnotationUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

public class ExtValCoreConfigurationTargetPropertyAnnotationTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationTargetPropertyAnnotationTestCase(String name)
    {
        super(name);
    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
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
                public Class<? extends Annotation> targetPropertyAnnotation()
                {
                    return CustomTargetProperty.class;
                }

            }, true);
        }
    }

    public void testTargetPropertyAnnotationDefault() throws Exception
    {
        Method method = ReflectionUtils.getMethod(ConstraintSourceUtils.class,
                "getTargetPropertyMetaData", Class.class, String.class);
        assertNotNull(method);
        Annotation target = (Annotation) ReflectionUtils.invokeMethodOfClass(
                ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property3");

        assertEquals("test1", ExtValAnnotationUtils.extractValueOf(target,
                Object.class));

        assertNull(ReflectionUtils.invokeMethodOfClass(
                ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property4"));
    }

    public void testTargetPropertyAnnotationCustomConfig() throws Exception
    {
        Method method = ReflectionUtils.getMethod(ConstraintSourceUtils.class,
                "getTargetPropertyMetaData", Class.class, String.class);
        assertNotNull(method);
        Annotation target = (Annotation) ReflectionUtils.invokeMethodOfClass(
                ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property4");

        assertEquals("test2", ExtValAnnotationUtils.extractValueOf(target,
                Object.class));

        assertNull(ReflectionUtils.invokeMethodOfClass(
                ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property3"));
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationTargetPropertyAnnotationTestCase.class);
    }

}
