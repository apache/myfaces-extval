package org.apache.myfaces.extensions.validator.test.base.util;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.MalformedURLException;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.runner.TestCaseClassLoader;

/**
 * Allows the execution of each test of a TestCase into a different classloader.
 * Specify the class of the test case in the constructor and return the instance
 * of the ClassLoaderTestSuite as the result of the suite method.
 * 
 * Most of the code is borrowed from junit.framework.TestSuite.
 * 
 * @author Rudy De Busscher
 * 
 */
@SuppressWarnings("unchecked")
public class ClassLoaderTestSuite extends TestSuite
{
    private static final Logger LOGGER = Logger.getLogger(ClassLoaderTestSuite.class.getName());
    
    // The classpath is needed because the custom class loader looks their to find the classes.
    private static String classPath;
    private static boolean classPathDetermined = false;

    public ClassLoaderTestSuite(Class theClass)
    {
        identifyTestMethods(theClass);

    }

    /**
     * Identify test methods.
     * 
     * @param theClass
     *            the class
     */
    private void identifyTestMethods(Class theClass)
    {
        Class superClass = theClass;
        Vector names = new Vector();
        while (Test.class.isAssignableFrom(superClass))
        {
            Method[] methods = superClass.getDeclaredMethods();
            for (int i = 0; i < methods.length; i++)
            {
                addTestMethod(methods[i], names, theClass);
            }
            superClass = superClass.getSuperclass();
        }
    }

    /**
     * Adds the method as a test method when it fulfill all requirements.
     * 
     * @param method
     *            the method that is maybe a test method
     * @param names
     *            the names with all the discovered test methods
     * @param theClass
     *            the class
     */
    private void addTestMethod(Method method, Vector names, Class theClass)
    {
        String name = method.getName();
        if (names.contains(name))
        {
            return;
        }
        if (!isPublicTestMethod(method))
        {
            if (isTestMethod(method))
            {
                addTest(warning("Test method isn't public: " + method.getName()));
            }
            return;
        }
        names.addElement(name);
        addTest(createTestWithCustomClassLoader(theClass, name));
    }

    /**
     * The method creates a Test using a custom classloader for each test.
     * 
     * @param theOriginalClass
     *            The class
     * @param name
     *            The test method name
     * @return The test where class is now loaded from a custom class loader.
     */
    static public Test createTestWithCustomClassLoader(Class theOriginalClass, String name)
    {
        
        ClassLoader classLoader;

        String surefireTestPath = getClassPath();
        if (surefireTestPath != null)
        {
            classLoader = new TestCaseClassLoader(surefireTestPath);
        }

        else
        {
            classLoader = new TestCaseClassLoader();

        }

        Class theClass = null;
        try
        {
            // Use the custom classloader to load the test. The complete
            // execution of the test will then be done using this new
            // classloader.
            theClass = classLoader.loadClass(theOriginalClass.getName());
        } catch (ClassNotFoundException e)
        {
            return warning("Cannot custom load test case: " + name + " (" + exceptionToString(e) + ")");
        }

        Constructor constructor;
        try
        {
            constructor = getTestConstructor(theClass);
        } catch (NoSuchMethodException e)
        {
            return warning("Class " + theClass.getName()
                    + " has no public constructor TestCase(String name) or TestCase()");
        }
        Object test;
        try
        {
            if (constructor.getParameterTypes().length == 0)
            {
                test = constructor.newInstance(new Object[0]);
                if (test instanceof TestCase)
                    ((TestCase) test).setName(name);
            } else
            {
                test = constructor.newInstance(new Object[] { name });
            }
        } catch (InstantiationException e)
        {
            return (warning("Cannot instantiate test case: " + name + " (" + exceptionToString(e) + ")"));
        } catch (InvocationTargetException e)
        {
            return (warning("Exception in constructor: " + name + " (" + exceptionToString(e.getTargetException())
                    + ")"));
        } catch (IllegalAccessException e)
        {
            return (warning("Cannot access test case: " + name + " (" + exceptionToString(e) + ")"));
        }
        return (Test) test;
    }

    /**
     * Gets the class path.This value is cached in a static variable for performance reasons.
     * 
     * @return the class path
     */
    private static String getClassPath()
    {
        if (classPathDetermined) {
            return classPath;
        }
        
        classPathDetermined = true;
        // running from maven, we have the classpath in this property.
        classPath = System.getProperty("surefire.test.class.path");
        if (classPath != null) {
            return classPath;
        }
        
        // For a multi module project, running it from the top we have to find it using another way.
        // We also need to set useSystemClassLoader=true in the POM so that we gets a jar with the classpath in it.
        String booterClassPath = System.getProperty("java.class.path");
        Vector pathItems = null;
        if (booterClassPath != null)
        {
            pathItems = scanPath(booterClassPath);
        }
        // Do we have just 1 entry as classpath which is a jar?
        if (pathItems != null && pathItems.size() == 1 && isJar((String) pathItems.get(0)))
        {
            classPath = loadJarManifestClassPath((String) pathItems.get(0), "META-INF/MANIFEST.MF");
        }
        return classPath;

    }

    /**
     * Load jar manifest class path.
     * 
     * @param path the path
     * @param fileName the file name
     * 
     * @return the string
     */
    private static String loadJarManifestClassPath(String path, String fileName)
    {
        File archive = new File(path);
        if (!archive.exists()) {
            return null;
        }
        ZipFile zipFile= null;

        try {
            zipFile= new ZipFile(archive);
        } catch(IOException io) {
            return null;
        }

        ZipEntry entry= zipFile.getEntry(fileName);
        if (entry == null)  {
            return null;     
        }
        try
        {
            Manifest mf = new Manifest();
            mf.read(zipFile.getInputStream(entry));

            return mf.getMainAttributes().getValue(
                    Attributes.Name.CLASS_PATH).replaceAll(" ", System.getProperty("path.separator")).replaceAll("file:/", "");
        } catch (MalformedURLException e)
        {
            LOGGER.throwing("ClassLoaderTestSuite", "loadJarManifestClassPath", e);
        } catch (IOException e)
        {
            LOGGER.throwing("ClassLoaderTestSuite", "loadJarManifestClassPath", e);
        }
        return null;
    }

    private static boolean isJar(String pathEntry)
    {
        return pathEntry.endsWith(".jar") || pathEntry.endsWith(".zip");
    }

    private static Vector scanPath(String classPath)
    {
        String separator = System.getProperty("path.separator");
        Vector pathItems = new Vector(10);
        StringTokenizer st = new StringTokenizer(classPath, separator);
        while (st.hasMoreTokens())
        {
            pathItems.addElement(st.nextToken());
        }
        return pathItems;
    }

    /**
     * Checks if is public test method.
     * 
     * @param method
     *            the method
     * 
     * @return true, if is public test method
     */
    private boolean isPublicTestMethod(Method method)
    {
        return isTestMethod(method) && Modifier.isPublic(method.getModifiers());
    }

    /**
     * Checks if is test method.
     * 
     * @param method
     *            the method
     * 
     * @return true, if is test method
     */
    private boolean isTestMethod(Method method)
    {
        String name = method.getName();
        Class[] parameters = method.getParameterTypes();
        Class returnType = method.getReturnType();
        return parameters.length == 0 && name.startsWith("test") && returnType.equals(Void.TYPE);
    }

    /**
     * Creates a Test that generates a failure with the supplied message.
     * 
     * @param message
     *            the message
     * 
     * @return the test
     */
    private static Test warning(final String message)
    {
        return new TestCase("warning")
        {
            protected void runTest()
            {
                fail(message);
            }
        };
    }

    /**
     * Exception to string.
     * 
     * @param t
     *            the t
     * 
     * @return the string
     */
    private static String exceptionToString(Throwable t)
    {
        StringWriter stringWriter = new StringWriter();
        PrintWriter writer = new PrintWriter(stringWriter);
        t.printStackTrace(writer);
        return stringWriter.toString();

    }

}
